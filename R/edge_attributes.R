#' Check if edge_attr_comb needs expansion or just validation
#'
#' If `edge_attr_comb` names are already correct, immediately pass to [combination_check()]
#'
#' @param graph An igraph
#' @param edge_attr_comb Named list of combination functions
#'
#' @return A valid named list of combination functions
#' @noRd
evaluate_combinators <- function(graph, edge_attr_comb) {
  if (all(names(edge_attr_comb) %in% edge_attr_names(graph))) {
    combination_check(graph, edge_attr_comb)
  } else {
    fill_combinators(graph, edge_attr_comb)
  }
}

#' Expand a supplied list of edge attribute combiners
#'
#' The resulting list will match all the edge attribute names of the initial
#' graph and no longer contain a `.default.combiner` value, to allow clean
#' processing later.
#'
#' @param graph An igraph
#' @param edge_attr_comb An incomplete or empty list of edge combiner functions
#' @return A valid named list of comination functions
#' @noRd
fill_combinators <- function(graph, edge_attr_comb) {
  if (is.null(edge_attr_comb[[".default.combiner"]])) {
    stop("If not all edge attribute names are specified in edge_attr_comb,
         then a .default.combiner function MUST be specified.")
  }

  unmentioned_edges <- setdiff(edge_attr_names(graph), names(edge_attr_comb))
  defaulted_edges <- lapply(unmentioned_edges, function(x) edge_attr_comb[[".default.combiner"]])
  names(defaulted_edges) <- unmentioned_edges
  res <- c(edge_attr_comb, defaulted_edges)
  res[[".default.combiner"]] <- NULL
  combination_check(graph, res)
}

#' Validate that all edge combinations match the given graph
#'
#' Will stop on errors
#'
#' @param graph An igraph
#' @param edge_attr_comb Full named list of edge combiners
#'
#' @return `edge_attr_comb`
#' @noRd
combination_check <- function(graph, edge_attr_comb) {
  mapply(function(attr_name, attr_comb_fun) {
    stopifnot(attr_name %in% edge_attr_names(graph))
    stopifnot(is.function(attr_comb_fun))
  }, names(edge_attr_comb), edge_attr_comb)
  edge_attr_comb
}

#' New edge attributes given list of merges
#'
#' Loops through each original edge attribute vector and applies the specified
#' combination function from `edge_attr_comb` along the list of `merges`
#' accumulated during [prune_loop()].
#'
#' @param orig_edge_attrs data.frame. Original graph edge attributes
#' @param merges A [`queue`][dequer::queue] of lists with `new` merged edge
#'   index and two `old` edge indices
#' @param edge_attr_comb Named list of edge combiners
#'
#' @return A data.frame of merged edge attributes. Still contains edges that
#'   have been removed; this will be subset in the next step of
#'   [simplify_topology()].
#' @noRd
produce_new_edges <- function(orig_edge_attrs, merges, edge_attr_comb) {
  merge_list <- as.list(merges)
  merge_results <- mapply(function(attr_name, attr_comb_fun) {
    playback_merges(merge_list, orig_edge_attrs[[attr_name]], sumfun = attr_comb_fun)
  }, names(edge_attr_comb), edge_attr_comb, SIMPLIFY = FALSE)
  as.data.frame(merge_results, stringsAsFactors = FALSE)
}

#' Apply edge merges to an edge attribute vector
#'
#' Takes an original edge attribute vector and applies `sumfun` based on the sequence of edge merges.
#'
#' @param merge_list List. Sequence of edge merges.
#' @param v A vector.
#' @param sumfun A function. Must take a vector and return a scalar of the same type.
#' @noRd
playback_merges <- function(merge_list, v, sumfun) {
  for (i in seq_along(merge_list)) {
    merge <- merge_list[[i]]
    v[merge[["new"]]] <- sumfun(v[merge[["old"]]])
  }
  v
}

#' First value in a vector
#'
#' This is modeled on dplyr's [`first`][dplyr::first]. Used as the default edge
#' attribute combination function in [simplify_topology()].
#'
#' @param v A vector.
#' @return The first value of `v`.
#'
#' @export
first <- function(v) utils::head(v, 1)
