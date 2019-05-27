library(igraph)
library(dequer)
set.seed(100)
# Necessary for pre-R-3.6.0 results
suppressWarnings({
RNGkind(sample.kind = "Rounding")
})

base_graph <- graph_from_edgelist(
  matrix(c(
    "a", "b",
    "b", "f",
    "b", "d",
    "d", "c",
    "d", "e",
    "e", "k",
    "j", "k",
    "f", "g",
    "c", "h",
    "h", "i",
    "f", "j"
  ), ncol = 2, byrow = TRUE),
  directed = FALSE
)

graph <- base_graph

vertex_attr(graph, "node_id") <- month.abb[seq_len(vcount(graph))]
edge_attr(graph, "edge_id") <- month.name[seq_len(ecount(graph))]
edge_attr(graph, "distance") <- sample(1:10, size = ecount(graph), replace = TRUE)

