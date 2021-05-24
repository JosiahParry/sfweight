#' Identify polygon neighbors
#'
#' @param x An sf or sfc object.
#' @param queen Default `TRUE`. For more see `?spdep::poly2nb`
#' @importFrom spdep poly2nb
#' @export
st_neighbors <- function(x, queen = TRUE, ...) {
  nb <- poly2nb(x, queen = queen, ...)

  unclass(nb)
}

#' Pure Higher Order Neighbors
#'
#' @param neighbors A neighbor list object as created by `st_neighbors()`.
#' @param order The order of neighbors.
#' @importFrom spdep nblag
#' @export
st_neighbor_lag <- function(neighbors, order) {
  class(neighbors) <- "nb"

  unclass(nblag(neighbors, order)[[order]])
}

#' Encompassing Higher Order Neighbors
#'
#' Creates an encompassing neighbor list of the order specified.
#' For example, if the order is 2 the result contains both 1st
#' and 2nd order neighbors.
#'
#' @inheritParams st_neighbor_lag
#' @importFrom spdep nblag_cumul nblag
#' @export
st_neighbor_lag_cumul <- function(neighbors, order) {
  class(neighbors) <- "nb"

  unclass(nblag_cumul(nblag(neighbors, order)))

}




