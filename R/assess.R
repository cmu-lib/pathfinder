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
assess_path <- function(pathway) {
  starting_point <- pathway$starting_point
  ending_point <- pathway$pathfinding_results$point
  n_steps <- length(pathway$epath)
  edge_itinerary <- unlist(pathway$epath)
  bundle_itinterary <- unlist(pathway$bpath)

  total_distance <- sum(distances[edge_itinerary])
  times_bundles_crossed <- tibble::enframe(table(bundle_itinterary), name = "bundle.id", value = "n")
  times_bundles_crossed$bundle.id <- as.integer(as.character(times_bundles_crossed$bundle.id))

  max_times_crossed <- max(times_bundles_crossed$n)
  bundle_most_crossed <- times_bundles_crossed$bundle.id[which.max(times_bundles_crossed$n)]
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
