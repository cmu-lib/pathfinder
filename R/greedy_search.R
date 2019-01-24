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
#' @param edge_bundles. A list of integer vectors. Each list item will be considered one edge bundle, and each vector the edge indices belonging to that bundle.
#' @param quiet Boolean. Display progress?
#' @param distances Double. A vector of length `ecount(graph)` with the distances of each edge.
#' @param penalize Boolean. Penalize the edge distance of already-crossed bundles to discourage crossing the same bundles more than once?
#'
#' @return An object
#' @export
greedy_search <- function(graph, starting_point = 1, edge_bundles, quiet = !interactive(), distances, penalize = TRUE) {
  search_set <- get_interface_points(graph)
  stopifnot(starting_point %in% search_set)

  # Create queues to hold edgelist, nodelist, and bridgelist, since we don't know
  # how long they need to be ahead of time.
  qe <- dequer::queue()
  qv <- dequer::queue()
  qb <- dequer::queue()

  # Start by crossing a bridge. This function will recurse until it runs out of
  # pathways or runs out of bridge termini to hit.
  pathfinding_results <- locate_next_path(
    graph = graph,
    starting_point = starting_point,
    search_set = search_set,
    qe = qe, qv = qv, qb = qb,
    is_bridge_crossing = TRUE,
    quiet = quiet)

  vpath = as.list(qv)
  epath = as.list(qe)

  results <- structure(
    list(
      epath = epath,
      vpath = vpath,
      pathfinding_results = pathfinding_results,
      starting_point = starting_point
    ),
    class = "pathfinder_path")
}
