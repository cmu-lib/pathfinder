#' Remove all 2-degree nodes from a graph
#'
#' Rewires `graph` to remove all 2-degree nodes. Nodes in the new graph will
#' retain their original attributes. New edges will inherit attributes from the
#' cominbation of their parent edges.
#'
#' Specify this combination by providing a named list of summary functions.
#' These summary functions must take a vector and return a scalar value of the
#' same type. For example:
#'
#' `list(distance = sum, edge_id = max, .default.combiner = min)`
#'
#' The value of `.default.combiner` will be used for any edge attributes that
#' aren't specified elsewhere in the list.
#'
#' @param graph An undirected [`igraph`][igraph::igraph] object
#' @param edge_attr_comb Named list mapping edge attribute merging functions.
#'   See Details.
#' @param protected_nodes Integer. Indices of nodes that should NOT be removed
#' @param progress Boolean. Display progress bar?
#'
#' @import igraph dequer
#'
#' @return An undirected [`igraph`][igraph::igraph]
#' @export
simplify_topology <- function(graph, edge_attr_comb = list(.default.combiner = first), protected_nodes = NULL, progress = interactive()) {
  stopifnot(!is.directed(graph))

  # Validate edge attribute combination spec and fill out default values
  edge_attr_comb <- evaluate_combinators(graph, edge_attr_comb)

  if (!is.null(protected_nodes)) stopifnot(all(protected_nodes %in% seq_len(vcount(graph))))

  # Extract original graph data
  original_mat <- as_adjacency_matrix(graph, type = "upper", edges = TRUE, sparse = TRUE)
  orig_edge_attrs <- as_data_frame(graph, what = "edges")[,-c(1:2), drop = FALSE]
  orig_vertex_attrs <- as_data_frame(graph, what = "vertices")

  merges <- queue()
  working_mat <- prune_loop(original_mat, protected_nodes, merges, progress)

  # Eliminate all ndoes without any connections
  connected_nodes <- which((Matrix::rowSums(working_mat) + Matrix::colSums(working_mat)) > 0)
  pruned_mat <- working_mat[connected_nodes, connected_nodes]

  # Form new graph and attach edge attributes
  new_graph <- graph_from_adjacency_matrix(pruned_mat, mode = "upper", weighted = "newindex")
  new_edge_attrs <- produce_new_edges(orig_edge_attrs, merges, edge_attr_comb)[pruned_mat@x,, drop = FALSE]
  new_vertex_attrs <- orig_vertex_attrs[connected_nodes,, drop = FALSE]
  edge.attributes(new_graph) <- as.list(new_edge_attrs)
  vertex.attributes(new_graph) <- as.list(new_vertex_attrs)

  new_graph
}

#' Rewrite adjacency matrix by merging edges
#'
#' @param m An adjacency matrix; values are edge indices
#' @param protected_nodes Integer. Node indices not to be deleted
#' @param merges An empty [`queue`][dequer::queue]
#' @param progress Boolean. Show progress
#'
#' @return Rewritten adjancecy matrix
#' @noRd
prune_loop <- function(m, protected_nodes, merges, progress) {
  stopifnot(!anyDuplicated(m@x))
  removable <- setdiff(get_degree_2(m), protected_nodes)
  i <- 1

  if (progress)
    pb <- progress::progress_bar$new("[:bar] :percent eta: :eta", total = length(removable) + 0.1 * length(removable), width = 60)
  while (i <= length(removable)) {
    if (progress) pb$tick()
    ri <- removable[i]
    # pull edges and figure out new edge pair
    edge_connections <- m[ri,] + m[,ri]
    nodes_to_unite <- which(edge_connections > 0)

    # Under certain conditions, after several rounds of editing the matrix, the
    # original calculation of 2-degree nodes will no longer be correct. In this
    # case, recalculate the remaining 2-degree nodes in the current matrix and
    # restart the loop.
    if (length(nodes_to_unite) != 2) {
      i <- 1
      removable <- setdiff(get_degree_2(m), protected_nodes)
      next()
    }

    # Mark a new edge with the index value of the first edge removed. These
    # indices will later be used to generate the new edge attribute table
    replacement_index <- edge_connections[nodes_to_unite[1]]
    x_coord <- pmin.int(nodes_to_unite[1], nodes_to_unite[2])
    y_coord <- pmax.int(nodes_to_unite[1], nodes_to_unite[2])
    m[x_coord, y_coord] <- replacement_index

    # Record which edges have been merged alongside the replacement index
    edges_merged <- list(new = replacement_index, old = edge_connections[nodes_to_unite])
    pushback(merges, edges_merged)

    # Axe the old node
    m[ri, nodes_to_unite] <- 0
    m[nodes_to_unite, ri] <- 0

    i <- i + 1
  }

  m
}

#' Get indices of degree-2 nodes
#'
#' @param m A distance matrix
#' @return An integer vector
#' @noRd
get_degree_2 <- function(m) {
  which((Matrix::colSums(m > 0) + Matrix::rowSums(m > 0)) == 2)
}
