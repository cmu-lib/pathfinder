#' Edge weight penalty functions
#'
#' @param x Numeric. Edge distance
#'
#' @return Numeric the same length as `x`
#'
#' @name penalize
NULL

#' @rdname penalize
penalize_square <- function(x) {
  (x + 1000)^2
}

#' @rdname penalize
penalize_inf <- function(x) {
  rep(Inf, times = length(x))
}

get_bundled_edges <- function(edge_bundles) {
  res <- unlist(edge_bundles)
  bundle_ids <- unlist(mapply(rep, seq_along(edge_bundles), vapply(edge_bundles, length, FUN.VALUE = integer(1))))
  attr(res, "pathfinder.bundle_ids") <- bundle_ids
  res
}

#' Find the vertices in a graph thatwhere bundled and non-bundled edges are tangent
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

#' @export
unique_pathway_edges <- function(pathway) {
  assert_that(inherits(pathway, "pathfinder_path"))
  unique(unlist(pathway$epath))
}

#' @export
pathway_steps <- function(pathway) {
  assert_that(inherits(pathway, "pathfinder_path"))
  res <- vapply(pathway$vpath, function(x) utils::head(x, 1), FUN.VALUE = integer(1))
  tibble::tibble(
    node_id = res,
    step = seq_along(res)
  )
}
