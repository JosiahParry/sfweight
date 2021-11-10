#' Conducts a Geary's C test
#'
#' @param x A numeric vector.
#' @inheritParams recreate_listw
#' @family stats
#' @export
geary_test <- function(x, neighbors, weights, ...) {
  listw <- recreate_listw(neighbors, weights)

  geary.test(x, listw, ...)
}


