# Any function to send a message will do nothing if quiet = TRUE

#' @import glue
message_end <- function(quiet, ...) {
  if (!quiet) message(glue(
    "Completed a path with {length(epath)} steps crossing {length(unlist(epath))} edges."))
}

step_status_message <- function(quiet, starting_point, search_set, is_bundle_crossing, bundle_id = NULL) {
  starting <- glue("Starting from {starting_point}.")
  points_left <- glue("{length(search_set)} candidate points left.")

  if (is_bundle_crossing) {
    glue("{starting} Crossing bundle {bundle_id}. {points_left}")
  } else {
    glue("{starting} Looking for roads. {points_left}")
  }
}
