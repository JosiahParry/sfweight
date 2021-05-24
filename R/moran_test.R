#' Conduct a Moran's I test
#' @importFrom spdep moran.test
#' @export
moran_test <- function(x, neighbors, weights, ...) {
  listw <- recreate_listw(neighbors, weights)

  moran.test(x, listw, ...)
}

#' Calculate the Local Moran's I Statistic

#' @param x A numeric vector.
#' @inheritParams recreate_listw
#' @param ... See `?spdep::localmoran()` for more options.
#' @importFrom spdep localmoran
#' @export
local_moran <- function(x, neighbors, weights, ...) {
  listw <- recreate_listw(neighbors, weights)
  lmoran <- localmoran(x, listw, ...)

  df <- as.data.frame(lmoran)

  setNames(df, c("ii", "e_ii", "var_ii", "z_ii", "p_ii"))

}



#' Categorize LISA
#'
#' @param x Numeric vector.
#' @param x_lag The spatial lag of x as calculated by `st_lag()`.
#' @export
categorize_lisa <- function(x, x_lag, scale = TRUE) {

  cats <- character(length(x))

  if (scale) {
    x <- scale(x)
    x_lag <- scale(x_lag)

    cats[x > 0 & x_lag > 0] <- "HH"
    cats[x > 0 & x_lag < 0] <- "HL"
    cats[x < 0 & x_lag < 0] <- "LL"
    cats[x < 0 & x_lag > 0] <- "LH"

  }

  cats[x > mean(x) & x_lag > mean(x_lag)] <- "HH"
  cats[x > mean(x) & x_lag < mean(x_lag)] <- "HL"
  cats[x < mean(x) & x_lag < mean(x_lag)] <- "LL"
  cats[x < mean(x) & x_lag > mean(x_lag)] <- "LH"

  cats
}


