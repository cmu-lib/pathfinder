context("test-utils")

test_that("Edge bundles", {
  try_bundled_edges <- get_bundled_edges(edge_bundles)
  expect_equivalent(try_bundled_edges, c(1514L, 2847L, 2252L, 3112L, 2815L, 2836L, 2837L, 2838L, 2853L,
                 2855L, 2856L, 3105L, 3055L, 3095L, 3685L, 3686L))
  expect_equal(attr(try_bundled_edges, "pathfinder.bundle_ids"), c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 6L,
                                          6L))
  expect_is(try_bundled_edges, "integer")
  expect_equal(anyDuplicated(try_bundled_edges), 0L)
})

test_that("Interface vertices are detected", {
  ip1 <- get_interface_points(graph, bundled_edges)
  expect_equal(ip1, c(585L, 1156L, 885L, 1271L, 1144L, 1152L, 1158L, 1159L, 1160L,
                      1269L, 1247L, 1265L, 1545L, 1546L, 1537L, 1526L))
})
