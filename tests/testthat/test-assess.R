context("test-assess")

test_that("asseessment functions return data frames", {
  pathway <- greedy_search(pgh_graph, pgh_bundles, pgh_distances, quiet = TRUE)
  test_glance <- glance(pathway)
  test_augment_edges <- augment_edges(pathway)
  test_augment_path <- augment_path(pathway)

  expect_is(test_glance, "tbl_df")
  expect_is(test_augment_edges, "tbl_df")
  expect_is(test_augment_path, "tbl_df")
})
