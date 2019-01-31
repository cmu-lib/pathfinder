context("test-greedy_search")

test_that("greedy_search checks inputs", {
  expect_error(greedy_search(1, pgh_bundles = 1, distances = 1))
  expect_error(greedy_search(pgh_graph, pgh_bundles, pgh_distances, starting_point = c(1, 2)))
  expect_error(greedy_search(pgh_graph, pgh_bundles, pgh_distances, penalize = "a"))
  expect_error(greedy_search(pgh_graph, pgh_bundles, pgh_distances, quiet = "a"))
})

test_that("greedy_search returns filled list", {
  expect_message(pathway <- greedy_search(pgh_graph, pgh_bundles, pgh_distances, starting_point = 1L, quiet = FALSE), regexp = "\\d", all = TRUE)
  expect_equivalent(pathway$starting_point, 1L)
  expect_equivalent(length(pathway$pathfinding_results$search_set), 0L)
  expect_gt(length(pathway$epath), 1L)
  expect_gt(length(pathway$vpath), 1L)
})
