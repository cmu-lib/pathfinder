context("test-assess")

test_that("asseessment functions return data frames", {
  pathway <- greedy_search(graph, edge_bundles, distances, quiet = TRUE)
  test_glance <- glance(pathway)
  test_tidy <- tidy(pathway)
})
