context("test-simplify_topology")

test_that("simplify_topology doesn't balk at featureless graphs", {
  expect_silent(simplify_topology(base_graph, progress = FALSE))
})

test_that("simplify_toplogy returns a graph without 2-degree nodes", {
  simp_graph <- simplify_topology(graph)
  expect_equivalent(vcount(simp_graph), 6)
  expect_equivalent(ecount(simp_graph), 6)
  expect_true(all(degree(simp_graph) != 2))
  expect_equivalent(lapply(vertex.attributes(simp_graph), class), lapply(vertex.attributes(graph), class))
})

test_that("simplify_toplogy respects protected nodes", {
  expect_error(simplify_topology(graph, protected_nodes = c(6, 100)))
  simp_graph <- simplify_topology(graph, protected_nodes = c(6, 10))
  expect_equivalent(vcount(simp_graph), 8)
  expect_equivalent(ecount(simp_graph), 8)
  expect_equivalent(vertex_attr(simp_graph, "name"), c("a", "b", "f", "d", "e", "g", "h", "i"))
  expect_equivalent(lapply(vertex.attributes(simp_graph), class), lapply(vertex.attributes(graph), class))
})

test_that("simplify_toplogy respects edge attribute combination", {
  simp_graph <- simplify_topology(graph, edge_attr_comb = list(edge_id = min, distance = sum))
  preserved_simp_graph <- simplify_topology(graph, edge_attr_comb = list(edge_id = min, distance = sum), protected_nodes = c(6, 10))

  expect_equal(sum(edge_attr(graph, "distance")), sum(edge_attr(simp_graph, "distance")))
  mapply(expect_equivalent, edge_attr(simp_graph, "distance"), c(4, 3, 6, 26, 4, 9))
  expect_equivalent(lapply(vertex.attributes(simp_graph), class), lapply(vertex.attributes(graph), class))

  expect_equal(sum(edge_attr(graph, "distance")), sum(edge_attr(preserved_simp_graph, "distance")))
  expect_gt(vcount(preserved_simp_graph), vcount(simp_graph))
})

test_that("playback_merges deals with edge variables correctly", {
  v <- 1:5
  merge_list <- list(
    list(new = 2, old = c(2, 3)),
    list(new = 2, old = c(2, 1))
  )
  expect_equal(playback_merges(merge_list, v, sumfun = sum), c(1, 6, 3, 4, 5))
  expect_equal(playback_merges(merge_list, v, sumfun = min), c(1, 1, 3, 4, 5))
  expect_equal(playback_merges(merge_list, v, sumfun = function(x) head(x, n = 1)), c(1, 2, 3, 4, 5))
})

test_that("produce_new_edges", {
  orig_edge_attrs <- as.data.frame(edge.attributes(graph), stringsAsFactors = FALSE)
  eac <- list(edge_id = function(x) tail(x, n = 1), distance = sum)
  bad_oac <- list(edge_id = sum)
  merges <- as.queue(list(
    list(new = 2, old = c(2, 3)),
    list(new = 2, old = c(2, 1))
  ))
  expect_error(produce_new_edges(orig_edge_attrs, merges, edge_attr_comb = bad_oac))
  res <- produce_new_edges(orig_edge_attrs, merges, edge_attr_comb = eac)
  expect_is(res, "data.frame")
  expect_equal(nrow(res), nrow(orig_edge_attrs))
  expect_equal(colnames(res), colnames(orig_edge_attrs))
  expect_equal(res[1:2, 1], c("January", "January"))
  expect_equal(res[, 2], c(4, 13, 6, 1, 5, 5, 9, 4, 6, 2, 7))
})

test_that("combination_check", {
  expect_error(combination_check(graph, list("not_an_edge")))
  expect_error(combination_check(graph, list("edge_id" = sum, "distance" = "this")))
})

test_that("evaluate_combinators", {
  expect_silent(evaluate_combinators(graph, edge_attr_comb = list(.default.combiner = first)))
  expect_silent(evaluate_combinators(graph, edge_attr_comb = list(distance = sum, .default.combiner = first)))
  expect_error(evaluate_combinators(graph, edge_attr_comb = list(foo = min, .default.combiner = sum)))
})
