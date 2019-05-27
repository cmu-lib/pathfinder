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
