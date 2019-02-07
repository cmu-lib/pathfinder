#' Pittsburgh road network
#'
#' @format An [`igraph`] object containing a sample of street network data from Pittsburgh.
#'
#' @source [OpenStreetMap](https://www.openstreetmap.org), available under the Open Database License, © OpenStreetMap contributors.
"pgh_graph"

#' Pittsburgh bridge bundles
#'
#' Each vector groups edge indices belonging to the same bridge.
#'
#' @format A list of edge index vectors from [pgh_graph].
#'
#' @source [OpenStreetMap](https://www.openstreetmap.org), available under the Open Database License, © OpenStreetMap contributors.
"pgh_bundles"


#' Pittsburgh edge distances
#'
#' @format A numeric vector of edge distances for [pgh_graph] in meters.
#'
#' @source [OpenStreetMap](https://www.openstreetmap.org), available under the Open Database License, © OpenStreetMap contributors.
"pgh_distances"
