#' Edge weight penalty function
penalize <- function(x) {
  (x + 1000)^2
}

get_bundled_edges <- function(edge_bundles) {
  res <- unlist(edge_bundles)
  bundle_ids <- unlist(mapply(rep, seq_along(edge_bundles), vapply(edge_bundles, length, FUN.VALUE = integer(1))))
  attr(res, "pathfinder.bundle_ids") <- bundle_ids
  res
}

is_interface_vertex <- function(graph, vid, bundled_edges) {
  incidence <- as.integer(incident(graph, vid)) %in% bundled_edges
  any(incidence) & any(!incidence)
}

get_bundle_interfaces <- function(graph, edge_bundle, bundled_edges) {
  all_vertices <- unique(as.integer(ends(graph, edge_bundle, names = FALSE)))
  is_interface <- vapply(all_vertices, function(x) is_interface_vertex(graph, x, bundled_edges), FUN.VALUE = logical(1))
  all_vertices[is_interface]
}

get_interface_points <- function(graph, edge_bundles) {
  res <- parallel::parLapply(cl = cl, edge_bundles, function(x) get_bundle_interfaces(graph, x, get_bundled_edges(edge_bundles)))
  unlist(res)
}

unique_pathway_edges <- function(pathway) {
  assert_that(inherits(pathway, "pathfinder_path"))
  unique(unlist(pathway$epath))
}

pathway_steps <- function(pathway) {
  assert_that(inherits(pathway, "pathfinder_path"))
  res <- vapply(pathway$vpath, function(x) head(x, 1), FUN.VALUE = integer(1))
  tibble::tibble(
    node_id = res,
    step = seq_along(res)
  )
}
