context("test-decorators")

test_that("Decorators add graph attributes", {
  expect_error(decorate_graph(NA_character_, edge_bundles, distances))
  expect_error(decorate_graph(graph, edge_bundles, letters))
  expect_error(decorate_graph(graph, edge_bundles, 1:100))
  expect_error(decorate_graph(graph, edge_bundles, c(1:10, NA_integer_, 12:5272)))
  expect_error(decorate_graph(graph, distances, distances))

  dg <- decorate_graph(graph, edge_bundles, distances)

  expect_equal(edge_attr(dg, "pathfinder.distance"), distances)

  expect_is(edge_attr(dg, "pathfinder.required"), "logical")
  expect_true(noNA(edge_attr(dg, "pathfinder.required")))

  expect_is(edge_attr(dg, "pathfinder.bundle_id"), "integer")
  expect_true(all(na.omit(edge_attr(dg, "pathfinder.bundle_id")) %in% seq_along(edge_bundles)))

  expect_is(vertex_attr(dg, "pathfinder.interface"), "logical")
  expect_true(noNA(vertex_attr(dg, "pathfinder.interface")))
})
