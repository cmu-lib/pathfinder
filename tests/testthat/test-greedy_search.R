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
  expect_equivalent(pathway$remaining_search_set, integer(0))
  expect_gt(length(pathway$epath), 1L)
  expect_gt(length(pathway$vpath), 1L)
})

test_that("greedy_search works without penalty", {
  expect_message(unpenalized_pathway <- greedy_search(pgh_graph, pgh_bundles, pgh_distances, starting_point = 1L, penalize = FALSE, quiet = FALSE), regexp = "\\d", all = TRUE)
  expect_equivalent(unpenalized_pathway$starting_point, 1L)
  expect_equivalent(unpenalized_pathway$remaining_search_set, integer(0))
  expect_gt(length(unpenalized_pathway$epath), 1L)
  expect_gt(length(unpenalized_pathway$vpath), 1L)
})


test_that("greedy_search works with infinte penalty", {
  expect_warning(inf_pathway <- greedy_search(pgh_graph, pgh_bundles, pgh_distances, starting_point = 1L, penalty_fun = penalize_inf, quiet = TRUE), regexp = "Stopped early")
  expect_equivalent(inf_pathway$starting_point, 1L)
  expect_gt(length(inf_pathway$remaining_search_set), 0L)
  expect_gt(length(inf_pathway$epath), 1L)
  expect_gt(length(inf_pathway$vpath), 1L)
  expect_equivalent(anyDuplicated(unlist(inf_pathway$bpath)), 0)
})

test_that("bad penalty function raises error", {
  expect_error(greedy_search(pgh_graph, pgh_bundles, pgh_distances, starting_point = 1L, penalty_fun = function(x, i) return(1:10), quiet = TRUE), regexp = "vector of distances the same length as the number of edge indices passed to it")
})
