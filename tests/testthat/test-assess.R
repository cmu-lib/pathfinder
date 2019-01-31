context("test-assess")

test_that("asseessment functions return data frames", {
  pathway <- greedy_search(graph, edge_bundles, distances, quiet = TRUE)
  test_glance <- glance(pathway)
  test_tidy <- tidy(pathway)
  test_augment <- augment(pathway)

  expect_is(test_glance, "tbl_df")
  expect_is(test_tidy, "tbl_df")
  expect_is(test_augment, "tbl_df")
})
