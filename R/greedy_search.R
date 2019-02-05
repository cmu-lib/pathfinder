#' Greedy search for bundled edges
#'
#' This strategy uses a greedy search along the original graph. It will navigate
#' to a node tangent to the nearest edge bundle, cross that bundle, then find
#' the next closest bundle, and so on, until at least one edge in each of the
#' specified bundles has been visited.
#'
#' The penalty function will make crossing any edge in an already-crossed bundle
#' extremely unattractive to the pathfinding function. By setting the edge
#' distance to a very high, but still finite, number, this will allow the path
#' to cross and already-traversed edge bundle again if there are no other
#' options available.
#'
#' @param graph An [igraph::igraph] object.
#' @param starting_point Integer. Index of the point from which to start the
#'   search.
#' @param edge_bundles A list of integer vectors. Each list item will be
#'   considered one edge bundle, and each vector the edge indices belonging to
#'   that bundle.
#' @param distances Double. A vector of length `ecount(graph)` with the
#'   distances of each edge.
#' @param penalize Boolean. Penalize the edge distance of already-crossed
#'   bundles to discourage crossing the same bundles more than once?
#' @param penalty_fun A function to penalize the weights of all edges in a
#'   bundle any time at least one edge in that bundle is traversed. Defaults to
#'   [`penalty_square`], however can be set to [`penalty_inf`] to set weight to
#'   `Inf` and thus forbid recrossing a bundle. Using [`penalty_inf`] can result
#'   in an incomplete path. See [`penalize`] for guidance on supplying your own
#'   function. Ignored if `penalize = FALSE`.
#' @param quiet Boolean. Display progress?
#'
#' @import assertthat
#'
#' @return A `pathfinder` object, which is a list with the following elements:
#' - `epath`: A list of integer vectors representing the successive edges
#' crossed by the path
#' - `vpath`: A list of integer vectors representing the successive vertices
#' crossed by the path
#' - `bpath`: A list of integer vectors representing the successive edge bundles
#' crossed by the path
#'
#' @export
greedy_search <- function(graph, edge_bundles, distances, starting_point = 1, penalize = TRUE, penalty_fun = penalize_square, quiet = !interactive()) {
  pathfinder_graph <- decorate_graph(graph, edge_bundles, distances)
  assert_that(is.count(starting_point))
  assert_that(starting_point %in% seq_len(vcount(pathfinder_graph)), msg = "starting_point must be an index of a vertex in graph")
  assert_that(is.flag(penalize))
  assert_that(is.flag(quiet))

  # Create queues to hold edgelist, nodelist, and bundlelist, since we don't know
  # how long they need to be ahead of time.
  qe <- dequer::queue()
  qv <- dequer::queue()
  qb <- dequer::queue()

  # Collect all interface points on graph that will potentially need to be visited
  search_set <- which(vertex_attr(pathfinder_graph, "pathfinder.interface"))

  # Is the starting point on an edge bundle?
  is_edge_bundle <- starting_point %in% search_set

  pathfinding_results <- greedy_search_handler(
    pathfinder_graph = pathfinder_graph,
    starting_point = starting_point,
    search_set = search_set,
    qe = qe, qv = qv, qb = qb,
    is_bundle_crossing = is_edge_bundle,
    penalize = penalize,
    penalty_fun = penalty_fun,
    quiet = quiet
  )

  vpath <- as.list(qv)
  epath <- as.list(qe)
  bpath <- as.list(qb)

  pathway <- structure(
    list(
      epath = epath,
      vpath = vpath,
      bpath = bpath,
      edge_bundles = edge_bundles,
      distances = distances,
      starting_point = starting_point,
      ending_point = pathfinding_results[["end_point"]],
      path_is_complete = pathfinding_results[["path_is_complete"]],
      remaining_search_set = pathfinding_results[["search_set"]]
    ),
    class = "pathfinder_path"
  )

  if (pathway$path_is_complete) {
    message_end(quiet, pathway = pathway)
  } else if (!pathway$path_is_complete) {
    message_break(quiet, pathway = pathway)
  }
  return(pathway)
}


#' Pathfinding handler
#'
#' Primary recursive function that steps through the graph. This function has two
#' modes depending on the value of is_bundle_crossing:
#'
#' - If TRUE, the function tries to cross the the farthest node THAT BELONGS TO
#' THE SAME bundle, almost always ensuring that it successfully crosses the
#' bundle.
#'
#' - If FALSE, the function looks for the next nearest point that is the entry
#' point for a bundle, and takes that path.
#'
#' Regardless of which mode the function is using, it saves both the node & edge
#' paths to the queues, and then checks which bundles have been crossed by the
#' path it just took. For those bundles, it 1) increases ALL the bundle edge
#' weights (not just the weights of the bundles crossed) by squaring them, making
#' them exponentially less-attractive for future crossings; and 2) removes any of
#' the associated nodes for that bundle from the remaining set of nodes to be
#' visited.
#'
#' After this, it passes the newly reweighted graph and the new starting point to
#' itself, and flips its mode. It will recurse until search_set is empty, or
#' until it can find no further paths to take.
#'
#' @import igraph dequer
greedy_search_handler <- function(pathfinder_graph, starting_point, search_set, qe, qv, qb, is_bundle_crossing, penalize, penalty_fun = penalize_square, quiet) {
  assertthat::assert_that(inherits(pathfinder_graph, "pathfinder_graph"),
    msg = "graph must be decorated with pathfinder attributes. Call decorate_graph() first."
  )

  while (length(search_set) > 0) {
    crossed_bundles <- unlist(as.list(qb))
    search_set <- setdiff(search_set, starting_point)

    starter_ids <- get_candidate_points(pathfinder_graph, starting_point, search_set, is_bundle_crossing, crossed_bundles)
    candidate_points <- starter_ids$candidate_points
    candidate_edges <- starter_ids$candidate_edges
    bundle_id <- starter_ids$bundle_id

    # If the candidate point lengths are 0, this means the point may likely be
    # tangent to another bundle and so it's not inherited both associated bundle
    # IDs. In this case, allow pathfinding to seek out the next closest bundle.
    if (length(candidate_points) == 0) {
      message_none(quiet)
      is_bundle_crossing <- FALSE
      bundle_id <- NULL
      candidate_points <- search_set
    }

    # Report on status
    step_status_message(quiet, starting_point, search_set, is_bundle_crossing, bundle_id)

    candidate_distances <- get_candidate_distances(pathfinder_graph, starting_point, candidate_points, candidate_edges, is_bundle_crossing)

    if (all(is.infinite(candidate_distances$candidate_distances))) {
      warning("Not all points reachable. Stopped early.\n")
      return(
        list(
          is_bundle_crossing = is_bundle_crossing,
          path_is_complete = FALSE,
          end_point = starting_point,
          search_set = search_set
        )
      )
    }

    possible_paths <- get_possible_paths(pathfinder_graph, candidate_distances$candidate_distances, candidate_distances$bundle_graph, starting_point, candidate_points, candidate_edges, is_bundle_crossing)

    if (!is.null(possible_paths$break_reason)) return(possible_paths)

    epath <- possible_paths$epath[[1]]$pathfinder.edge_id
    vpath <- as.integer(possible_paths$vpath[[1]])

    pushback(qe, epath)
    pushback(qv, vpath)

    bundles_crossed <- stats::na.omit(unique(edge_attr(pathfinder_graph, "pathfinder.bundle_id", index = epath)))

    if (length(bundles_crossed) > 0) {
      # Any bundles crossed get added to the queue
      pushback(qb, bundles_crossed)
      message_crossed(quiet, bundles_crossed)

      bundle_edges <- which(edge_attr(pathfinder_graph, "pathfinder.bundle_id") %in% bundles_crossed)

      if (penalize == TRUE) {
        # ALL edges belonging to the bundle, whether they were actually crossed or
        # not, get penalized, and their nodes removed from the search list
        message_increase(quiet, bundle_edges)
        new_distances <- penalty_fun(edge_attr(pathfinder_graph, "pathfinder.distance"), bundle_edges)
        assert_that(length(new_distances) == length(bundle_edges), msg = "penalty_fun must return a vector of distances the same length as the number of edge indices passed to it")
        edge_attr(pathfinder_graph, "pathfinder.distance", index = bundle_edges) <- new_distances
      }

      # Remove all nodes on the crossed bundle from the remaining search set
      bundle_nodes <- union(
        as.integer(head_of(pathfinder_graph, es = bundle_edges)),
        as.integer(tail_of(pathfinder_graph, es = bundle_edges))
      )
      removed_nodes <- intersect(search_set, bundle_nodes)
      if (length(removed_nodes) > 0) {
        search_set <- setdiff(search_set, bundle_nodes)
        message_removed(quiet, removed_nodes, search_set)
      }
    }

    # Pass two items to the next search step:
    # 1) the final node of the vpath - this becomes the starting point for the next step
    # 2) the pruned search set that removes all the nodes from the bundle just considered
    starting_point <- utils::tail(vpath, 1)
    is_bundle_crossing <- !is_bundle_crossing
  }

  # If all bundles have been reached, return out empty set
  return(
    list(
      is_bundle_crossing = is_bundle_crossing,
      path_is_complete = TRUE,
      end_point = starting_point,
      search_set = search_set
    )
  )
}

get_candidate_points <- function(pathfinder_graph, starting_point, search_set, is_bundle_crossing, crossed_bundles) {
  if (is_bundle_crossing) {
    # Get first uncrossed bundle that goes from this node. This logic is
    # required when a potential node is tangent to two different bridge
    # relations. It will cause the uncrossed relation to be favored.
    bundle_id <- utils::head(setdiff(stats::na.omit(
      edge_attr(pathfinder_graph, "pathfinder.bundle_id",
        index = E(pathfinder_graph)[.from(starting_point)]
      )
    ), crossed_bundles), 1)
    candidate_edges <- which(edge_attr(pathfinder_graph, "pathfinder.bundle_id") == bundle_id)
    # Collect the head/"to" nodes of all the bundle edges, since we will always
    # be starting at the tail/"from" node of a bundle edge
    candidate_points <- setdiff(unique(
      as.integer(head_of(pathfinder_graph, es = candidate_edges))
    ), starting_point)
  } else {
    bundle_id <- NULL
    candidate_points <- search_set
    candidate_edges <- NULL
  }

  list(
    bundle_id = bundle_id,
    candidate_points = candidate_points,
    candidate_edges = candidate_edges
  )
}

get_candidate_distances <- function(pathfinder_graph, starting_point, candidate_points, candidate_edges, is_bundle_crossing) {
  if (is_bundle_crossing) {
    bundle_graph <- subgraph.edges(pathfinder_graph, eids = candidate_edges, delete.vertices = FALSE)
    candidate_distances <- distances(bundle_graph, v = starting_point, to = candidate_points, mode = "out", weights = edge_attr(bundle_graph, "pathfinder.distance"))
  } else {
    bundle_graph <- NULL
    candidate_distances <- distances(pathfinder_graph,
      v = starting_point, to = candidate_points, mode = "out",
      weights = edge_attr(pathfinder_graph, "pathfinder.distance")
    )
  }

  list(
    candidate_distances = candidate_distances,
    bundle_graph = bundle_graph
  )
}

get_possible_paths <- function(pathfinder_graph, candidate_distances, bundle_graph, starting_point, candidate_points, candidate_edges, is_bundle_crossing) {
  assertthat::assert_that(!is.null(candidate_distances))

  if (is_bundle_crossing) {
    # Find a path within the subgraph comprising only that bundle
    ranking <- rank(candidate_distances, ties.method = "min")
    ranking[is.infinite(candidate_distances)] <- 0L
    target_point <- candidate_points[which.max(ranking)]
    suppressWarnings({
      possible_paths <- shortest_paths(
        bundle_graph,
        from = starting_point, to = target_point,
        mode = "out", output = "both",
        weights = edge_attr(bundle_graph, "pathfinder.distance")
      )
    })
  } else {
    # Find the closest point and try navigating to it
    target_point <- candidate_points[which.min(candidate_distances)]
    suppressWarnings({
      possible_paths <- shortest_paths(pathfinder_graph,
        from = starting_point, to = target_point,
        weights = edge_attr(pathfinder_graph, "pathfinder.distance"), output = "both", mode = "out"
      )
    })
  }

  return(possible_paths)
}
