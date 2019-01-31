#' Utility function that applies edge and vertex attributes to a graph that will
#' be used by pathfinder and then later removed.
#'
#' @inheritParams greedy_search
#'
#' @return A `pathfinder_graph` object.
#'
#' @import assertthat igraph
decorate_graph <- function(graph, edge_bundles, distances) {
  assert_that(inherits(graph, "igraph"), msg = "pathfinder currently only works with igraph objects")
  assert_that(is.numeric(distances), msg = "distances must be a numeric vector")
  assert_that(length(distances) == ecount(graph), msg = "distances must have the same length as there are edges in graph")
  assert_that(noNA(distances), msg = "No NAs are allowed in distances")
  assert_that(is.list(edge_bundles), msg = "edge_bundles must be a list of integer vectors")
  lapply(edge_bundles, function(x) {
    assert_that(all(x %in% seq_len(ecount(graph))),
                msg = "All integers in each edge_bundles list element must match to edge indices in graph")
  })

  bundled_edges <- get_bundled_edges(edge_bundles)

  edge_attr(graph, "pathfinder.edge_id") <- seq_len(ecount(graph))
  edge_attr(graph, "pathfinder.distance") <- distances
  edge_attr(graph, "pathfinder.required") <- FALSE
  edge_attr(graph, "pathfinder.required", index = bundled_edges) <- TRUE
  edge_attr(graph, "pathfinder.bundle_id") <- NA_integer_
  edge_attr(graph, "pathfinder.bundle_id", index = bundled_edges) <- attr(bundled_edges, "pathfinder.bundle_ids")

  vertex_attr(graph, "pathfinder.interface") <- FALSE
  vertex_attr(graph, "pathfinder.interface", index = get_interface_points(graph, edge_bundles)) <- TRUE

  class(graph) <- c(class(graph), "pathfinder_graph")
  graph
}
