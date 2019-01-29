#' @import ggraph igraph
#' @export
path_vis <- function(graph, pathway, edge_bundles) {
  stopifnot(inherits(graph, "igraph"))
  stopifnot(inherits(pathway, "pathfinder_path"))

  edge_attr(graph, "path_edge") <- FALSE
  edge_attr(graph, "path_edge", index = unique_pathway_edges(pathway)) <- TRUE
  lt <- create_layout(graph, "manual", node.positions = tibble::tibble(x = V(graph)$lon, y = V(graph)$lat))

  path_waypoints <- pathway_steps(pathway)
  path_waypoints$x <- lt$x[path_waypoints$node_id]
  path_waypoints$y <- lt$y[path_waypoints$node_id]

  ggraph(lt) +
    geom_node_point(alpha = 0.2) +
    geom_edge_link(aes(color = path_edge, edge_alpha = path_edge)) +
    scale_edge_color_manual(values = c("TRUE" = "red", "FALSE" = "gray10")) +
    ggrepel::geom_label_repel(data = path_waypoints, aes(x = x, y = y, label = step), box.padding = 1) +
    theme_graph()
}
