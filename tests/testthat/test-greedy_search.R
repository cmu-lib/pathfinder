context("test-greedy_search")

test_that("greedy_search checks inputs", {
  expect_error(greedy_search(1, pgh_bundles = 1, distances = 1))
  expect_error(greedy_search(pgh_graph, pgh_bundles, pgh_distances, starting_point = c(1, 2)))
  expect_error(greedy_search(pgh_graph, pgh_bundles, pgh_distances, quiet = "a"))
})

test_that("greedy_search returns filled list", {
  expect_message(pathway <- greedy_search(pgh_graph, pgh_bundles, pgh_distances, starting_point = 1L, cheat = TRUE, quiet = FALSE), regexp = "\\d")
  expect_equivalent(pathway$starting_point, 1L)
  expect_gt(length(pathway$epath), 1L)
  expect_true(pathway$path_is_complete)
})


test_that("greedy_search is quiet upon request", {
  expect_silent(quiet_pathway <- greedy_search(pgh_graph, pgh_bundles, pgh_distances, starting_point = 1L, quiet = TRUE))
})

