#' Identify polygon neighbors
#'
#' @param x An sf or sfc object.
#' @param queen Default `TRUE`. For more see `?spdep::poly2nb`
#' @importFrom spdep poly2nb
#' @family neighbors
#' @export
st_contiguity <- function(x, queen = TRUE, ...) {
  nb <- poly2nb(x, queen = queen, ...)

  unclass(nb)
}

#' Identify polygon neighbors
#' @description
#'
#' `r lifecycle::badge("superseded")`. Use `st_contiguity` instead. This function creates neighbors based on contiguities.
#'
#' @inheritParams st_contiguity
#' @keywords internal
#' @export
st_neighbors <- function(...) {
  lifecycle::deprecate_warn("0.0.0.9002", "st_neighbors()", "st_contiguity()")
  st_contiguity(...)
}

#' Pure Higher Order Neighbors
#'
#' @param nb A neighbor list object as created by `st_contiguity()`.
#' @param order The order of neighbors.
#' @importFrom spdep nblag
#' @family other
#' @export
st_nb_lag <- function(nb, order) {
  class(nb) <- "nb"

  unclass(nblag(nb, order)[[order]])
}

#' Encompassing Higher Order Neighbors
#'
#' Creates an encompassing neighbor list of the order specified.
#' For example, if the order is 2 the result contains both 1st
#' and 2nd order neighbors.
#'
#' @inheritParams st_neighbor_lag
#' @importFrom spdep nblag_cumul nblag
#' @family other
#' @export
st_nb_lag_cumul <- function(nb, order) {
  class(nb) <- "nb"

  unclass(nblag_cumul(nblag(nb, order)))

}




