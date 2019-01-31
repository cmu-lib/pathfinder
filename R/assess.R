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
#'
#' @export
glance <- function(pathway) {
  starting_point <- pathway$starting_point
  ending_point <- pathway$pathfinding_results$point
  n_steps <- length(pathway$epath)
  edge_itinerary <- unlist(pathway$epath)
  bundle_itinterary <- unlist(pathway$bpath)

  total_distance <- sum(distances[edge_itinerary])
  times_bundles_crossed <- bundle_cross_count(pathway$bpath)

  max_times_crossed <- max(times_bundles_crossed$n)
  bundle_most_crossed <- times_bundles_crossed$bundle_id[which.max(times_bundles_crossed$n)]
  mean_times_crossed <- mean(times_bundles_crossed$n)

  tibble::tibble(
    n_steps,
    starting_point,
    ending_point,
    total_distance,
    mean_times_crossed,
    max_times_crossed,
    bundle_most_crossed
  )
}

#' Returns a tidy data frame with one row per edge bundle
#'
#' @param pathway A `pathfinder_results` object from, e.g. [`greedy_search`].
#'
#' @return A [tibble::tibble] with the following columns
#'     - `bundle_id`
#'     - `times_crossed`
tidy <- function(pathway) {
  bundle_cross_count(pathway$bpath)
}

#' Returns a tidy data frame with one row per edge crossing
#'
#' @param pathway A `pathfinder_results` object from, e.g. [`greedy_search`].
#'
#' @return A [tibble::tibble] with the following columns
#'     - `index` Row number
#'     - `step_id` Step of the pathfinding algorithm
#'     - `edge_id` ID of edge crossed in this step (can be repeated)
#'     - `bundle_id` Id of bundle crossed in this step (`NA` if edge is not a bundle)
#'     - `times_edge_crossed` Cumulative times this edge has been crossed since the start of the path
#'     - `times_bundle_crossed` Cumulative times this bundle has been crossed since the start of the path
#' @importFrom dplyr mutate group_by ungroup select lag row_number
#' @import magrittr
#' @export
augment <- function(pathway) {
  suppressPackageStartupMessages({
    assertthat::assert_that(require(dplyr), msg = "augment() requires dplyr")
  })


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

  res %>%
    mutate(
      # True for any step that crosses onto a bridge from either a non-bridge or a different bridge
      bundle_switch = !is.na(bundle_id) & (is.na(lag(bundle_id)) | (lag(bundle_id) != bundle_id))) %>%
    group_by(edge_id) %>%
    mutate(times_edge_crossed = row_number()) %>%
    group_by(bundle_id) %>%
    mutate(times_bundle_crossed = cumsum(bundle_switch)) %>%
    ungroup() %>%
    select(-bundle_switch)
}

bundle_cross_count <- function(bpath) {
  res <- tibble::enframe(table(unlist(bpath)), name = "bundle_id", value = "n")
  res$bundle.id <- as.integer(as.character(res$bundle.id))
  res$n <- as.integer(res$n)
  res
}


