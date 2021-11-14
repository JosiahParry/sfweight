#' Conducts a Geary's C test
#'
#' @param x A numeric vector.
#' @inheritParams recreate_listw
#' @family stats
#' @export
geary_test <- function(x, nb, wt, ...) {
  listw <- recreate_listw(nb, wt)

  geary.test(x, listw, ...)
}


