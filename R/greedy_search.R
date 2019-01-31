#' Greedy search for bundled edges
#'
#' This strategy uses a greedy search along the original graph. It will navigate
#' to a node tangent to the nearest edge bundle, cross that bundle, then find
#' the next closest bundle, and so on, until at least one edge in each of the
#' specified bundles has been visited.
#'
#' The penalty function will make crossing any edge in an already-corssed bundle
#' extremely unattractive to the pathfinding function. By setting the edge
#' distance to a very high, but still finite, number, this will allow the path
#' to cross and already-traversed edge bundle again if there are no other
#' options available.
#'
#' @param graph An [igraph::igraph] object.
#' @param starting_point Integer. Index of the point from which to start the search.
#' @param edge_bundles A list of integer vectors. Each list item will be considered one edge bundle, and each vector the edge indices belonging to that bundle.
#' @param distances Double. A vector of length `ecount(graph)` with the distances of each edge.
#' @param penalize Boolean. Penalize the edge distance of already-crossed bundles to discourage crossing the same bundles more than once?
#' @param quiet Boolean. Display progress?
#'
#' @import assertthat
#'
#' @return A `pathfinder` object, which is a list with the following elements:
#' - `epath`: A list of integer vectors representing the successive edges crossed by the path
#' - `vpath`: A list of integer vectors representing the sucessive vertices crossed by the path
#' - `bpath`: A list of integer vectors representing the sucessive edge bundles crossed by the path
#'
#' @export
greedy_search <- function(graph, edge_bundles, distances, starting_point = 1, penalize = TRUE, quiet = !interactive()) {
  pathfinder_graph <- decorate_graph(graph, edge_bundles, distances)
  assert_that(is.count(starting_point))
  assert_that(starting_point %in% seq_len(vcount(pathfinder_graph)), msg = "starting_point must be an index of a vertex in graph")
  assert_that(is.flag(penalize))
  assert_that(is.flag(quiet))

  # Create queues to hold edgelist, nodelist, and bundlelist, since we don't know
  # how long they need to be ahead of time.
  q <- dequer::queue()
  qe <- dequer::queue()
  qv <- dequer::queue()
  qb <- dequer::queue()
  qd <- dequer::queue()

  # Collect all interface points on graph that will potentially need to be visited
  search_set <- which(vertex_attr(pathfinder_graph, "pathfinder.interface"))

  # Is the starting point on an edge bundle?
  is_edge_bundle <- starting_point %in% search_set

  pathfinding_results <- pathfind(
    pathfinder_graph = pathfinder_graph,
    starting_point = starting_point,
    search_set = search_set,
    q = q, qe = qe, qv = qv, qb = qb, qd = qd,
    is_bundle_crossing = is_edge_bundle,
    quiet = quiet)

  working_distances = as.list(q)
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
      ending_point = pathfinding_results$point
    ),
    class = "pathfinder_path")

  message_end(quiet, epath = pathway$epath)
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
pathfind <- function(pathfinder_graph, starting_point, search_set, q, qe, qv, qb, qd, is_bundle_crossing, quiet) {

  while (length(search_set) > 0) {

    crossed_bundles <- unlist(as.list(qb))
    search_set <- setdiff(search_set, starting_point)

    if (is_bundle_crossing) {
      # Get first uncrossed bundle that goes from this node. This logic is
      # required when a potential node is tangent to two different bridge
      # relations. It will cause the uncrossed relation to be favored.
      bundle_id <- head(setdiff(na.omit(edge_attr(pathfinder_graph, "pathfinder.bundle_id", E(pathfinder_graph)[.from(starting_point)])), crossed_bundles), 1)
      candidate_edges <- which(edge_attr(pathfinder_graph, "pathfinder.bundle_id") == bundle_id)
      # Collect the head/"to" nodes of all the bundle edges, since we will always
      # be starting at the tail/"from" node of a bundle edge
      candidate_points <- setdiff(unique(
        as.integer(head_of(pathfinder_graph, es = candidate_edges))), starting_point)
    } else {
      bundle_id <- NULL
      candidate_points <- search_set
    }

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

    if (is_bundle_crossing) {
      # Find a path within the subgraph comprising only that bundle
      bundle_graph <- subgraph.edges(pathfinder_graph, eids = candidate_edges, delete.vertices = FALSE)
      candidate_distances <- distances(bundle_graph, v = starting_point, to = candidate_points, mode = "out", weights = edge_attr(bundle_graph, "pathfinder.distance"))
      ranking <- rank(candidate_distances, ties.method = "min")
      ranking[is.infinite(candidate_distances)] <- 0L
      target_point <- candidate_points[which.max(ranking)]
      suppressWarnings({
        possible_paths <- shortest_paths(bundle_graph, from = starting_point, to = target_point, mode = "out", output = "both", weights = edge_attr(bundle_graph, "pathfinder.distance"))
      })
    } else {

      # Calculate possible distances
      candidate_distances <- distances(pathfinder_graph, v = starting_point, to = candidate_points, mode = "out",
                                       weights = edge_attr(pathfinder_graph, "pathfinder.distance"))

      # If no path can be found, then return out with warning
      if (all(is.infinite(candidate_distances)))
        return(list(
          is_bundle_crossing = is_bundle_crossing,
          break_reason = "No paths found to point",
          point = starting_point,
          candidates = candidate_points,
          search_set = search_set,
          distances = candidate_distances))

      # Find the closest point and try navigating to it
      target_point <- candidate_points[which.min(candidate_distances)]
      suppressWarnings({
        possible_paths <- shortest_paths(pathfinder_graph, from = starting_point, to = target_point,
                                         weights = edge_attr(pathfinder_graph, "pathfinder.distance"), output = "both", mode = "out")
      })
    }

    pushback(q, list(is_bundle_crossing = is_bundle_crossing, candidates = candidate_distances, chosen_path = possible_paths))
    epath <- possible_paths$epath[[1]]$pathfinder.edge_id
    distances <- possible_paths$epath[[1]]$pathfinder.distance
    vpath <- as.integer(possible_paths$vpath[[1]])

    pushback(qe, epath)
    pushback(qv, vpath)
    pushback(qd, distances)

    bundles_crossed <- na.omit(unique(edge_attr(pathfinder_graph, "pathfinder.bundle_id", index = epath)))
    recrossings <- which(bundles_crossed %in% crossed_bundles)
    if (length(recrossings) > 0) message(glue::glue("Bundles {paste(bundles_crossed[recrossings], collapse = ';')} recrossed!"))
    if (length(bundles_crossed) > 0) {
      # Any bundles crossed get added to the queue
      pushback(qb, bundles_crossed)
      message_crossed(quiet, bundles_crossed)

      # ALL edges belonging to the bundle, whether they were actually crossed or
      # not, get penalized, and their nodes removed from the search list
      bundle_edges <- which(edge_attr(pathfinder_graph, "pathfinder.bundle_id") %in% bundles_crossed)
      bundle_nodes <- union(as.integer(head_of(pathfinder_graph, es = bundle_edges)), as.integer(tail_of(pathfinder_graph, es = bundle_edges)))

      message_increase(quiet, bundle_edges)
      edge_attr(pathfinder_graph, "pathfinder.distance", index = bundle_edges) <- penalize(edge_attr(pathfinder_graph, "pathfinder.distance", index = bundle_edges))

      # Remove all nodes on the crossed bundle from the remaining search set
      removed_nodes <- intersect(search_set, bundle_nodes)
      if (length(removed_nodes) > 0) {
        search_set <- setdiff(search_set, bundle_nodes)
        message_removed(quiet, removed_nodes, search_set)
      }
    }

    # Pass two items to the next search step:
    # 1) the final node of the vpath - this becomes the starting point for the next step
    # 2) the pruned search set that removes all the nodes from the bundle just considered
    starting_point <- tail(vpath, 1)
    is_bundle_crossing = !is_bundle_crossing
  }

  # If all bundles have been reached, return out empty set
  return(
    list(
      is_bundle_crossing = is_bundle_crossing,
      break_reason = "All paths done",
      point = starting_point,
      candidates = candidate_points,
      search_set = search_set,
      distances = candidate_distances))
}
