#' @import glue
message_end <- function(quiet, pathway) {
  if (quiet) return(invisible())
  message(glue(
    "Completed a path with {length(pathway$epath)} steps crossing {length(unlist(pathway$epath))} edges and {length(unique(unlist(pathway$bpath)))} bundles."
  ))
}

#' @import glue
message_break <- function(quiet, pathway) {
  if (quiet) return(invisible())
  message(glue(
    "Path aborted early at {length(pathway$epath)} steps crossing {length(unlist(pathway$epath))} edges and {length(unique(unlist(pathway$bpath)))} bundles out of the {length(pathway$edge_bundles)} originally specified."
  ))
}


#' @import glue
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

#' @import glue
message_increase <- function(quiet, bundle_edges) {
  if (quiet) return(invisible())
  message("Increasing weights for edges: ", paste(bundle_edges, collapse = ";"))
}

#' @import glue
message_crossed <- function(quiet, bundles_crossed) {
  if (quiet) return(invisible())
  message("Bundle IDs crossed: ", paste(bundles_crossed, collapse = "; "))
}

#' @import glue
message_removed <- function(quiet, removed_nodes, search_set) {
  if (quiet) return(invisible())
  message(glue("removing nodes {paste(removed_nodes, collapse = '; ')}; {length(search_set)} nodes remaining."))
}

#' @import glue
message_none <- function(quiet) {
  if (quiet) return(invisible())
  message("0 bundle candidates, looking for a new point")
}
