#' Neighbors from a distance band
#'
#' Creates nieghbors based on a distance band.
#'
#' @param x An sf or sfc object.
#' @param lower The lower threshold of the distance band. It is recommended to keep this as 0.
#' @param upper The upper threshold of the distance band.
#' @param ... Passed to `spdep::dnearneigh()`.
#' @export
st_nb_band <- function(x, lower = 0, upper = .01, ...) {
  unclass(spdep::dnearneigh(x, lower, upper, ...))
}
