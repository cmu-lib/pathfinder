context("test-decorators")

test_that("Decorators add pgh_graph attributes", {
  expect_error(decorate_graph(NA_character_, pgh_bundles, pgh_distances))
  expect_error(decorate_graph(pgh_graph, pgh_bundles, letters))
  expect_error(decorate_graph(pgh_graph, pgh_bundles, 1:100))
  expect_error(decorate_graph(pgh_graph, pgh_bundles, c(1:10, NA_integer_, 12:5272)))
  expect_error(decorate_graph(pgh_graph, pgh_distances, pgh_distances))

  dg <- decorate_graph(pgh_graph, pgh_bundles, pgh_distances)

  expect_equal(edge_attr(dg, "pathfinder.distance"), pgh_distances)

  expect_equivalent(edge_attr(dg, "pathfinder.edge_id"), seq_len(ecount(pgh_graph)))

  expect_is(edge_attr(dg, "pathfinder.required"), "logical")
  expect_true(noNA(edge_attr(dg, "pathfinder.required")))

  expect_is(edge_attr(dg, "pathfinder.bundle_id"), "integer")
  expect_true(all(na.omit(edge_attr(dg, "pathfinder.bundle_id")) %in% seq_along(pgh_bundles)))

  expect_is(vertex_attr(dg, "pathfinder.interface"), "logical")
  expect_true(noNA(vertex_attr(dg, "pathfinder.interface")))
})
