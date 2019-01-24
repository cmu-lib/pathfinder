context("test-greedy_search")

test_that("greedy_search checks inputs", {
  expect_error(greedy_search(1, edge_bundles = 1, distances = 1))
  expect_error(greedy_search(graph, edge_bundles, distances, starting_point = c(1, 2)))
  expect_error(greedy_search(graph, edge_bundles, distances, penalize = "a"))
  expect_error(greedy_search(graph, edge_bundles, distances, quiet = "a"))
})
