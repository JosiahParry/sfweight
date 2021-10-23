#' Calculate spatial lag
#'
#' @param x A numeric vector
#' @param neighbors A neighbor list object as created by `st_neighbors()`.
#' @param weights A weights list as created by `st_weights()`.
#' @param allow_zero If `TRUE`, assigns zero as lagged value to zone without neighbors.
#' @param na_ok Default `FALSE`. If, `TRUE` missing values return a lagged `NA`.
#' @param ... See `?spdep::lag.listw` for more.
#' @importFrom spdep card
#' @export
st_lag <- function(x, neighbors, weights, allow_zero = NULL, na_ok = FALSE, ...) {

  class(neighbors) <- "nb"
  cardnb <- card(neighbors)

  .Call("lagw", neighbors, weights,
        x, as.integer(cardnb), as.logical(allow_zero),
        naok = na_ok, PACKAGE = "spdep")

}


