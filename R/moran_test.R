#' Conduct a Moran's I test
#' @importFrom spdep moran.test
#' @export
#' @examples
#' acs %>%
#'   mutate(nb = st_neighbors(geometry),
#'        wt = st_weights(nb)) %>%
#'   moran_test(bach, nb, wt)
moran_test <- function(data, x, neighbors, weights, ...) {

  listw <- recreate_listw(
    pull(data, {{ neighbors }}),
    pull(data, {{ weights }})
    )

  var <- pull(data, {{ x }})

  moran.test(var, listw, ...)
}

#' Calculate the Local Moran's I Statistic

#' @param x A numeric vector.
#' @inheritParams recreate_listw
#' @param ... See `?spdep::localmoran()` for more options.
#' @importFrom spdep localmoran
#' @export
#' @examples
#' library(tidyverse)
#'
#' lisa <- sfweight::acs %>%
#'   mutate(nb = st_neighbors(geometry),
#'          wt = st_weights(nb),
#'          moran = local_moran(med_house_income, nb, wt))
#'
#' pluck(lisa, "moran")
local_moran <- function(x, nb, wt, alpha = 0.05, scale = TRUE, ...) {
  listw <- recreate_listw(nb, wt)
  lmoran <- spdep::localmoran(x, listw, ...)

  df <- setNames(as.data.frame(lmoran),
                 c("ii", "e_ii", "var_ii", "z_ii", "p_ii"))

  lisa_cats <- categorize_lisa(x, spdep::lag.listw(listw, x))

  df[["lisa_category"]] <- ifelse(df[["p_ii"]] <= alpha, lisa_cats, "Insignificant")

  df
}

#' Unpack LISA data frame column
#'
#' @param x An sf object
#' @param lisa The unquoted column name containing the LISA data frame
#' @export
unpack_lisa <- function(x, lisa, ...) {
  # TODO check for tidyr & tibble fail is not installed
  sf::st_as_sf(tidyr::unpack(tibble::as_tibble(x), {{ lisa }}))
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
  cats[cats == ""] <- NA
  cats
}


