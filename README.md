
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pathfinder

[![Travis build
status](https://travis-ci.org/dSHARP-CMU/pathfinder.svg?branch=master)](https://travis-ci.org/dSHARP-CMU/pathfinder)
[![Coverage
status](https://codecov.io/gh/dSHARP-CMU/pathfinder/branch/master/graph/badge.svg)](https://codecov.io/github/dSHARP-CMU/pathfinder?branch=master)

The goal of pathfinder is to find a path across multiple edge bundles in
a graph that minimizes the number of times each bundle is traversed.

## Installation

You can install the development version:

``` r
devtools::install_github("dSHARP-CMU/pathfinder")
```

## Usage

Starting with an igraph object, specify edge bundles that must be
traversed by using a list of edge indices:

``` r
# A small subset of road data from Pittsburgh
pgh_graph
#> IGRAPH 6aae588 DN-- 2370 5272 -- 
#> + attr: name (v/c), lat (v/n), lon (v/n), label (v/c), component
#> | (v/n), is_interface (v/l), name (e/c), access (e/c), bridge
#> | (e/c), bridge_name (e/c), highway (e/c), label (e/c), oneway
#> | (e/c), bridge_relation (e/c), is_bridge (e/l), bridge_id (e/c),
#> | distance (e/n), within_boundaries (e/l), .id (e/n)
#> + edges from 6aae588 (vertex names):
#>  [1] 572065243->104186779 572065243->572065248 572065243->104186789
#>  [4] 104186779->572065243 104186779->104186773 104186779->104690428
#>  [7] 104186773->104186779 104186773->104186765 104186773->105931733
#> [10] 104186765->104186773 104186765->104704862 104377324->104377338
#> + ... omitted several edges

pgh_distances
#>   [1]  72.085183 175.888347  17.915673  72.085183  48.698138 127.009269
#>   [7]  48.698138 124.129447  53.534183 124.129447 174.141060  38.016235
#>  [13] 160.204296  49.830450  38.016235  41.572786 113.033133  41.572786
#>  [19]  46.552684 152.570640  49.833105  46.552684  47.849883 116.781708
#>  [25]  47.849883  46.385455  48.559679  46.385455  32.272382  49.021601
#>  [31] 116.781708 113.033133  76.333319  48.899350  76.333319  15.548177
#>  [37]   8.943212   3.699507   3.530424  44.029061  51.182060  49.230732
#>  [43]  42.271114 100.837279  42.271114  41.673987 105.822951 100.837279
#>  [49]  43.644899  50.846476  13.315098  43.644899  43.514411  51.525894
#>  [55]  43.514411  47.049973  11.626941 104.570919 121.503683  50.166166
#>  [61] 104.570919  49.143080 123.475677  38.145321  79.797690  63.128221
#>  [67]  38.145321  47.869854  64.966673  82.337796  47.869854   7.768423
#>  [73]  80.559962  47.888624  47.888624  53.260418  52.986123  82.969409
#>  [79]  52.750943  52.986123  52.260710  60.464955  51.940961  52.260710
#>  [85]  44.098179  52.763596 261.042196  27.025030  33.463659  52.763596
#>  [91] 127.389466  19.400097  32.272382  49.021601  67.025557   5.111531
#>  [97]  26.041358  48.619924  32.195773  34.663925
#>  [ reached getOption("max.print") -- omitted 5172 entries ]

# Edges belonging to Pittsburgh bridges
pgh_bundles
#> [[1]]
#> [1] 1514 2847
#> 
#> [[2]]
#> [1] 2252 3112
#> 
#> [[3]]
#> [1] 2815 2836 2837 2838
#> 
#> [[4]]
#> [1] 2853 2855 2856 3105
#> 
#> [[5]]
#> [1] 3055 3095
#> 
#> [[6]]
#> [1] 3685 3686
```

`greedy_search()` will traverse the graph by walking a path from the
starting vertex to the nearest edge bundle, crossing that bundle, then
finding the next closest bundle, until all bundles have been crossed at
least once. By setting `penalize = TRUE` you can add a severe edge
weight penalty to crossed bundles to discourage crossing bundles more
than
once.

``` r
penalized_run <- greedy_search(pgh_graph, edge_bundles = pgh_bundles, distances = pgh_distances, penalize = TRUE)
```

It’s also possible to supply penalty functions that will completely
prohibit recrossing edge bundles, however this may result in incomplete
paths that get
trapped.

``` r
infinite_run <- greedy_search(pgh_graph, edge_bundles = pgh_bundles, distances = pgh_distances, penalize = TRUE, penalty_fun = penalize_inf)
#> Warning in greedy_search_handler(pathfinder_graph = pathfinder_graph, starting_point = starting_point, : Not all points reachable. Stopped early.
```

Tidying methods are also available to get summary statistics on
resulting pathways at different levels of resolution. For more info, see
`?glance`, `?tidy`, `?augment`
