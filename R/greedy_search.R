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
#' @param graph An [`igraph`] object.
#' @param starting_point Integer. Index of the point from which to start the
#'   search.
#' @param edge_bundles A list of integer vectors. Each list item will be
#'   considered one edge bundle, and each vector the edge indices belonging to
#'   that bundle.
#' @param distances Double. A vector of length `ecount(graph)` with the
#'   distances of each edge.
#' @param cheat Boolean. If true, the search algorithm will temporarily treat
#'   the graph as undirected if it gets trapped behind a one-way edge, and try
#'   to restart its crawl.
#' @param quiet Boolean. Display progress?
#'
#' @import assertthat
#'
#' @return A `pathfinder_path` object, which is a list with the following elements:
#' - `epath`: A list of integer vectors representing the successive edges
#' crossed by the path
#' - `bpath`: A list of integer vectors representing the successive edge bundles
#' crossed by the path
#'
#' @export
greedy_search <- function(graph, edge_bundles, distances, starting_point = 1, cheat = TRUE, quiet = !interactive()) {
  pathfinder_graph <- decorate_graph(graph, edge_bundles, distances)
  assert_that(is.count(starting_point))
  assert_that(starting_point %in% seq_len(vcount(pathfinder_graph)), msg = "starting_point must be an index of a vertex in graph")
  assert_that(is.flag(quiet))

  pathway <- matrix_search(pathfinder_graph, starting_point = starting_point, cheat, quiet)

  pathway$edge_bundles <- edge_bundles

  if (pathway$path_is_complete) {
    message_end(quiet, pathway = pathway)
  } else if (!pathway$path_is_complete) {
    message_break(quiet, pathway = pathway)
  }
  return(pathway)
}
