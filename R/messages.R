#' Any function to send a message will do nothing if quiet = TRUE
#' @import glue
#' @name quiet_messaging
NULL

#' @rdname quiet_messaging
message_end <- function(quiet, epath) {
  if (quiet) return(invisible())
  message(glue(
    "Completed a path with {length(epath)} steps crossing {length(unlist(epath))} edges."))
}

#' @rdname quiet_messaging
step_status_message <- function(quiet, starting_point, search_set, is_bundle_crossing, bundle_id) {
  if (quiet) return(invisible())

  starting <- glue("Starting from {starting_point}.")
  points_left <- glue("{length(search_set)} candidate points left.")

  if (is_bundle_crossing) {
    glue("{starting} Crossing bundle {bundle_id}. {points_left}")
  } else {
    glue("{starting} Looking for roads. {points_left}")
  }
}

#' @rdname quiet_messaging
message_increase <- function(quiet, bundle_edges) {
  if (quiet) return(invisible())
  message("increasing weights for ", paste(bundle_edges, collapse = ";"))
}

#' @rdname quiet_messaging
message_crossed <- function(quiet, bundles_crossed) {
  if (quiet) return(invisible())
  message("bundles crossed: ", paste(bundles_crossed, collapse = "; "))
}

#' @rdname quiet_messaging
message_removed <- function(quiet, removed_nodes) {
  if (quiet) return(invisible())
  message("removing nodes ", paste(removed_nodes, collapse = "; "))
}

#' @rdname quiet_messaging
message_none <- function(quiet) {
  if (quiet) return(invisible())
  message("0 bundle candidates, looking for a new point")
}
