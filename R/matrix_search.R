# Matrix-search ----

#' Greedy search using precomputed matrices
#'
#' @param graph A graph
#' @param starting_point Integer. Starting vertex
#'
#' @export
matrix_search <- function(graph, starting_point = 1) {
  stopifnot(inherits(graph, "pathfinder_graph"))
  message("Pre-calculating distance matrices...", appendLF = FALSE)

  bridge_indices <- which(!is.na(edge_attr(graph, "pathfinder.bundle_id")))
  non_bridge_indices <- which(is.na(edge_attr(graph, "pathfinder.bundle_id")))
  no_bundle_graph <- delete_edges(graph, bridge_indices)
  bundle_only_graph <- delete_edges(graph, non_bridge_indices)


  edge_attr(graph, "pathfinder.distance", bridge_indices) <- 1e20


  message("full matrix...", appendLF = FALSE)
  full_matrix <- general_distance_matrix(graph)
  message("inter-bundle matrix...", appendLF = FALSE)
  no_bundle_matrix <- general_distance_matrix(no_bundle_graph)
  message("intra-bundle matrix...", appendLF = FALSE)
  bundle_only_matrix <- general_distance_matrix(bundle_only_graph)
  message("done.")

  points_to_visit <- !logical(nrow(bundle_only_matrix))
  names(points_to_visit) <- rownames(bundle_only_matrix)
  # Exlude from consideration any dead-end nodes (e.g. at the end of a one-way
  # edge) or nodes unreachable from the outside
  valid_targets <- rowSums(bundle_only_matrix, na.rm = TRUE) > 0 & colSums(no_bundle_matrix, na.rm = TRUE) > 0
  points_to_visit[!valid_targets] <- FALSE

  lookup_table <- node_sibling_lookup(graph)
  starting_point <- get_nearest_bridge_point(graph, points_to_visit, starting_point)

  message("Running greedy search...", appendLF = FALSE)
  step_list <- matrix_loop(full_matrix, bundle_only_matrix, no_bundle_matrix, points_to_visit, lookup_table, starting_point)
  message("done.")

  message("Extrapolating full paths...", appendLF = TRUE)
  full_path <- hydrate_path(graph, bundle_only_graph, no_bundle_graph, step_list)
  message("done.")

  pathway <- structure(list(
    epath = full_path[["epath"]],
    bpath = full_path[["bpath"]],
    step_types = vapply(step_list, function(x) x[["step"]], FUN.VALUE = character(1)),
    starting_point = as.integer(head(step_list, 1)[[1]][["from"]]),
    ending_point = as.integer(tail(step_list, 1)[[1]][["to"]]),
    cheated = vapply(step_list, function(x) x[["step"]] == "cheat", FUN.VALUE = logical(1)),
    graph_state = graph
  ), class = "pathfinder_path")

  pathway
}

#' @importFrom dequer queue pushback
#' @noRd
matrix_loop <- function(full_matrix, bundle_only_matrix, no_bundle_matrix, points_to_visit, lookup_table, starting_point) {
  q <- queue()

  while (sum(points_to_visit) > 1) {
    points_to_visit[lookup_table[[starting_point]]] <- FALSE

    # CROSS BUNDLE ----
    # message("Considering points ", paste0(b, collapse = "; "))
    intra_bridge_dist <- bundle_only_matrix[starting_point, ]

    # Any reachable nodes should be considered part of the bridge, and thus crossed
    b <- names(which(!is.na(intra_bridge_dist)))
    points_to_visit[b] <- FALSE

    target <- which.max(intra_bridge_dist)
    if (length(target) >= 1) {
      # proceed as normal
      global_from <- starting_point
      global_to <- names(target)
    } else {
      browser()
    }

    path_step <- list(
      from = global_from,
      to = global_to,
      step = "bridge"
    )

    pushback(q, path_step)

    # If crossing the bridge means we're done, then finish the loop
    if (sum(points_to_visit) == 0) break

    # CROSS GRAPH ----

    starting_point <- names(which.min(no_bundle_matrix[global_to, points_to_visit]))

    if (length(starting_point) == 1) {
      path_step <- list(
        from = global_to,
        to = starting_point,
        step = "road"
      )
    } else {
      # If none are available, cheat and find a new start point pathing via the full matrix
      starting_point <- names(which.min(full_matrix[global_from, points_to_visit]))
      path_step <- list(
        from = global_to,
        to = starting_point,
        step = "cheat"
      )
    }

    pushback(q, path_step)
    points_to_visit[starting_point] <- FALSE
    # If going to this final point means being done, then finish the loop
    if (sum(points_to_visit) == 0) break
  }

  return(as.list(q))
}

get_nearest_bridge_point <- function(graph, points_to_visit, starting_point) {
  working_points <- points_to_visit[points_to_visit]
  all_distances <- distances(graph, v = starting_point, to = as.integer(names(working_points)), mode = "out", weights = edge_attr(graph, "pathfinder.distance"))[1,]
  names(working_points)[which.min(all_distances)]
}

#' From a sequence of from-to points, generate a list of steps containing edge indices
#'
#' @param graph The original full graph
#' @param bundle_only_graph Graph with only bundle edges remaining
#' @param no_bundle_grpah Graph with all bundled edges removed
#' @param step_list Result from [matrix_loop()]
#'
#' @noRd
hydrate_path <- function(graph, bundle_only_graph, no_bundle_graph, step_list) {
  epath <- lapply(step_list, function(s) {
    s$from <- as.integer(s$from)
    s$to <- as.integer(s$to)

    if (s$step == "bridge") {

      p <- shortest_paths(bundle_only_graph, from = s$from, to = s$to, mode = "out",
                          weights = edge_attr(bundle_only_graph, "pathfinder.distance"), output = "epath")
      edge_attr(bundle_only_graph, "pathfinder.edge_id", p$epath[[1]])

    } else if (s$step == "road") {

      p <- shortest_paths(no_bundle_graph, from = s$from, to = s$to, mode = "out",
                          weights = edge_attr(no_bundle_graph, "pathfinder.distance"), output = "epath")
      edge_attr(no_bundle_graph, "pathfinder.edge_id", p$epath[[1]])

    } else if (s$step == "cheat") {

      p <- shortest_paths(graph, from = s$from, to = s$to, mode = "out",
                          weights = edge_attr(graph, "pathfinder.distance"), output = "epath")
      edge_attr(graph, "pathfinder.edge_id", p$epath[[1]])

    } else {
      stop("Invalid step type.")
    }
  })

  bundle_ids <- edge_attr(graph, "pathfinder.bundle_id")
  bpath <- lapply(epath, function(es) {
    unique(na.omit(bundle_ids[es]))
  })

  list(
    epath = epath,
    bpath = bpath
  )
}

# Precomputed-matrices ----

#' List of all interface nodes with their fellow bundled nodes
#'
#' For any given terminal node, find all its fellow terminal nodes for a given
#' edge bundle.
#'
#' @param graph A `pathfinder_graph` produced by [decorate_graph()]
#'
#' @return A named list of vertex indices as character
#'
#' @noRd
node_sibling_lookup <- function(graph) {
  interface_points <- as.character(which(vertex_attr(graph, "pathfinder.interface")))
  vertex_names <- vertex_attr(graph, "name")
  edf <- as_data_frame(graph, what = "edges")
  sliced_edf <- split(edf, f = as.factor(edf$pathfinder.bundle_id))
  bundle_points <- lapply(sliced_edf, function(df) unique(c(df[["from"]], df[["to"]])))
  bundle_indices <- lapply(bundle_points, function(pids) which(vertex_names %in% pids))

  res <- lapply(interface_points, function(vi) {
     intersect(interface_points, as.character(unique(unlist(lapply(bundle_indices, function(bps) if (vi %in% bps) bps), recursive = TRUE))))
  })
  names(res) <- interface_points
  res
}

#' @import igraph
#' @noRd
general_distance_matrix <- function(graph, lookup_table = NULL) {
  interface_points <- which(V(graph)$pathfinder.interface)
  orig_edge_ids <- edge_attr(graph, "pathfinder.edge_id")
  distance_matrix <- distances(graph,
                               v = interface_points,
                               to = interface_points,
                               mode = "out",
                               weights = E(graph)$pathfinder.distance)

  diag(distance_matrix) <- NA_real_
  colnames(distance_matrix) <- interface_points
  rownames(distance_matrix) <- interface_points
  distance_matrix[is.infinite(distance_matrix)] <- NA_real_

  if (!is.null(lookup_table)) {
    for (i in as.character(interface_points)) {
      exclusion_points <- lookup_table[[i]]
      distance_matrix[exclusion_points, exclusion_points] <- NA_real_
    }
  }

  # suppressWarnings({
  #   paths <- parallel::mclapply(interface_points, function(x) {
  #     shortest_paths(graph,
  #                    from = x,
  #                    to = interface_points,
  #                    mode = "out", output = "epath",
  #                    weights = E(graph)$pathfinder.distance)
  #   }, mc.preschedule = TRUE, mc.cores = 8)
  # })
  #
  # named_all_paths <- setNames(lapply(paths, function(x) {
  #   setNames(lapply(x$epath, function(y) {
  #     orig_edge_ids[as.integer(y)]
  #   }), interface_points)
  # }), interface_points)

  distance_matrix
}
