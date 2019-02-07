#' Edge weight penalty functions
#'
#' @param x Numeric. All edge distances from the graph
#' @param i Integer. The edge indices whose penalized weights will be returned.
#'
#' @return Numeric the same length as `i`.
#'
#' @name penalize
NULL

#' @describeIn penalize Add a constant based on the total edge distance in the graph and then squares the result.
#' @export
penalize_square <- function(x, i) {
  ((x[i]) + sum(x))^2
}

#' @describeIn penalize Make edge distances `Inf`
#' @export
penalize_inf <- function(x, i) {
  rep(Inf, times = length(i))
}

get_bundled_edges <- function(edge_bundles) {
  res <- unlist(edge_bundles)
  bundle_ids <- unlist(mapply(rep, seq_along(edge_bundles), vapply(edge_bundles, length, FUN.VALUE = integer(1))))
  attr(res, "pathfinder.bundle_ids") <- bundle_ids
  res
}

#' Find the vertices in a graph where bundled and non-bundled edges are tangent
#'
#' @inheritParams greedy_search
#'
#' @export
get_interface_points <- function(graph, edge_bundles) {
  bundled_edges <- get_bundled_edges(edge_bundles)
  non_bundled_edges <- setdiff(seq_len(ecount(graph)), bundled_edges)
  bundle_tangent <- unique(as.integer(ends(graph, es = bundled_edges, names = FALSE)))
  non_bundle_tangent <- unique(as.integer(ends(graph, es = non_bundled_edges, names = FALSE)))
  both_tangent <- intersect(bundle_tangent, non_bundle_tangent)
  both_tangent
}
