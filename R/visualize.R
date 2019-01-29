#' @import ggraph igraph
#' @export
path_vis <- function(graph, pathway) {
  stopifnot(inherits(graph, "igraph"))
  stopifnot(inherits(pathway, "pathfinder_results"))

  edge_attr(graph, "path_edge") <- FALSE
  edge_attr(graph, "path_edge", index = unique_pathway_edges(pathway)) <- TRUE
  lt <- create_layout(graph, "kk")

  ggraph(lt) +
    geom_node_point() +
    geom_edge_link(aes(color = path_edge, edge_width = path_edge)) +
    geom_edge_co
    theme_graph()
}
