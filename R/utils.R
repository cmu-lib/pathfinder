#' Bridge weight penalty function
penalize <- function(x) {
  (x + 1000)^2
}

#' Status message per step
step_status_message <- function(starting_point, search_set, is_bridge_crossing, bridge_id = NULL) {
  starting <- str_glue("Starting from {starting_point}.")
  points_left <- str_glue("{length(search_set)} candidate points left.")

  if (is_bridge_crossing) {
    str_glue("{starting} Crossing bridge {bridge_id}. {points_left}")
  } else {
    str_glue("{starting} Looking for roads. {points_left}")
  }
}

get_interface_points <- function(graph, edge_bundles) {
  which(V(graph)$is_interface)
}
