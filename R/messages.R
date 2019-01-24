# Any function to send a message will do nothing if quiet = TRUE

#' @import glue
message_end <- function(quiet, ...) {
  if (!quiet) message(glue(
    "Completed a path with {length(epath)} steps crossing {length(unlist(epath))} edges."))
}
