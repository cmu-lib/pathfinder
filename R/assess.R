#' Return a tidy data frame with metrics on the performance of a pathway
#'
#' @param pathway A `pathfinder_results` object from, e.g. [`greedy_search`].
#'
#' @return A one-row [tibble::tibble] with the following columns
#'   - `n_steps` Number of steps
#'   - `starting_point` Starting point index
#'   - `ending_point` Ending point index
#'   - `total_distance` Sum of all distances of crossed edges
#'   - `mean_times_crossed` Mean number of times bundles were crossed
#'   - `max_times_crossed` Maximum number of times bundles were crossed
#'   - `bundle_most_crossed` Bundle id with the highest number of crossings.
#'   - `p_multiple_crossed` Proportion of bundles crossed more than once.
#'
#' @export
glance <- function(pathway) {
  assertthat::assert_that(inherits(pathway, "pathfinder_path"))

  starting_point <- pathway$starting_point
  ending_point <- pathway$ending_point
  n_steps <- length(pathway$epath)
  edge_itinerary <- unlist(pathway$epath)
  bundle_itinterary <- unlist(pathway$bpath)

  total_distance <- sum(pathway$distances[edge_itinerary])
  times_bundles_crossed <- bundle_cross_count(pathway$bpath)

  max_times_crossed <- max(times_bundles_crossed$n)
  bundle_most_crossed <- times_bundles_crossed$bundle_id[which.max(times_bundles_crossed$n)]
  mean_times_crossed <- mean(times_bundles_crossed$n)
  p_multiple_crossed <- sum(times_bundles_crossed$n > 1) / length(times_bundles_crossed$n)

  tibble::tibble(
    n_steps,
    starting_point,
    ending_point,
    total_distance,
    mean_times_crossed,
    max_times_crossed,
    bundle_most_crossed,
    p_multiple_crossed
  )
}

#' Returns a tidy data frame with one row per edge bundle
#'
#' @param pathway A `pathfinder_results` object from, e.g. [`greedy_search`].
#'
#' @return A [tibble::tibble] with the following columns
#'   - `bundle_id`
#'   - `times_crossed`
#'
#' @export
tidy <- function(pathway) {
  assertthat::assert_that(inherits(pathway, "pathfinder_path"))

  bundle_cross_count(pathway$bpath)
}

#' Returns a tidy data frame with one row per edge crossing
#'
#' @param pathway A `pathfinder_results` object from, e.g. [`greedy_search`].
#'
#' @return A [tibble::tibble] with the following columns:
#'   - `index` Row number
#'   - `step_id` Step of the pathfinding algorithm
#'   - `edge_id` ID of edge crossed in this step (can be repeated)
#'   - `bundle_id` Id of bundle crossed in this step (`NA` if edge is not a bundle)
#'   - `times_edge_crossed` Cumulative times this edge has been crossed since the start of the path
#'   - `times_bundle_crossed` Cumulative times this bundle has been crossed since the start of the path
#' @importFrom dplyr filter mutate group_by ungroup select lag row_number distinct left_join
#' @import magrittr
#' @export
augment <- function(pathway) {
  assertthat::assert_that(inherits(pathway, "pathfinder_path"))

  all_edges <- unlist(pathway$epath)
  step_id <- unlist(mapply(function(e, i) rep(i, times = length(e)), pathway$epath, seq_along(pathway$epath)))
  bundled_edges <- get_bundled_edges(pathway$edge_bundles)

  index <- seq_along(all_edges)
  edge_id <- all_edges
  bundle_id <- attr(bundled_edges, "pathfinder.bundle_ids")[match(all_edges, bundled_edges)]

  res <- tibble::tibble(
    index,
    step_id,
    edge_id,
    bundle_id)

  # Count up distinct times a bridge is crossed for each step
  bridge_steps <- res %>%
    filter(!is.na(bundle_id)) %>%
    distinct(step_id, bundle_id) %>%
    group_by(bundle_id) %>%
    mutate(times_bundle_crossed = row_number()) %>%
    ungroup()

  res %>%
    group_by(edge_id) %>%
    mutate(times_edge_crossed = row_number()) %>%
    left_join(bridge_steps, by = c("step_id", "bundle_id"))
}

bundle_cross_count <- function(bpath) {
  res <- tibble::enframe(table(unlist(bpath)), name = "bundle_id", value = "n")
  res$bundle_id <- as.integer(as.character(res$bundle_id))
  res$n <- as.integer(res$n)
  res
}

#' Get pathway data
#'
#' @inheritParams glance
#' @name pathway
NULL

#' @describeIn pathway Get unique edges visited by pathway
#' @export
unique_pathway_edges <- function(pathway) {
  assert_that(inherits(pathway, "pathfinder_path"))
  unique(unlist(pathway$epath))
}

#' @describeIn pathway Get a list of the nodes visited by a pathway
#' @export
pathway_steps <- function(pathway) {
  assert_that(inherits(pathway, "pathfinder_path"))
  res <- vapply(pathway$vpath, function(x) utils::head(x, 1), FUN.VALUE = integer(1))
  tibble::tibble(
    node_id = res,
    step = seq_along(res)
  )
}
