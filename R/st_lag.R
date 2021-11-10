#' Calculate spatial lag
#'
#' @param x A numeric vector
#' @param nb A neighbor list object as created by `st_neighbors()`.
#' @param wt A weights list as created by `st_weights()`.
#' @param allow_zero If `TRUE`, assigns zero as lagged value to zone without neighbors.
#' @param na_ok Default `FALSE`. If, `TRUE` missing values return a lagged `NA`.
#' @param ... See `?spdep::lag.listw` for more.
#' @importFrom spdep card
#' @family stats
#' @export
st_lag <- function(x, nb, wt, allow_zero = NULL, na_ok = FALSE, ...) {

  class(nb) <- "nb"
  cardnb <- card(nb)

  .Call("lagw", nb, wt,
        x, as.integer(cardnb), as.logical(allow_zero),
        naok = na_ok, PACKAGE = "spdep")

}


#'
#'
#' #' Calculate arbitrary lags
#' #'
#' #'
#' #'
#' st_apply <- function(x, nb, wt, .f, ...) {
#'   f <- as_mapper(.f, ...)
#'   map2(nb, wt,  ~{
#'     f(x[.x], ...)
#'   })
#' }
#'
#' # This works
#' wt <- acs_nb$wt
#' nb <- acs_nb$nb
#' imap(acs_nb$nb, ~{
#'   .nb <- .x
#'   .wt <- wt[[.y]]
#'
#'   sum(acs_nb$med_house_income[.nb] * .wt)
#'
#' })
#'
#'
#' x <- acs_nb$med_house_income
#' imap(acs_nb$nb, ~{
#'   .nb <- .x
#'   .wt <- wt[[.y]]
#'   .xj <- x[.nb]
#'
#'   .xj
#'   #f(.xj, .wt)
#'   #sum(acs_nb$med_house_income[.nb] * .wt)
#'
#' })
