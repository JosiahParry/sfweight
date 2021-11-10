#' Conduct a Moran's I test
#' @importFrom spdep moran.test
#' @export
#' @family stats
#' @examples
#' acs %>%
#'   mutate(nb = st_neighbors(geometry),
#'        wt = st_weights(nb)) %>%
#'   moran_test(bach, nb, wt)
moran_test <- function(data, x, nb, wt, ...) {

  listw <- recreate_listw(
    pull(data, {{ nb }}),
    pull(data, {{ wt }})
    )

  var <- pull(data, {{ x }})

  moran.test(var, listw, ...)
}


#' Moran's I Permutation Test
#'
#' @importFrom spdep moran.mc
#' @family stats
#' @export
moran_mc <- function(data, x, nb, wt, nsim = 999, alternative = "greater", ...) {

  listw <- recreate_listw(
    pull(data, {{ nb }}),
    pull(data, {{ wt }})
  )

  var <- pull(data, {{ x }})

  moran.mc(var, listw, nsim = nsim, alternative = alternative, ...)

}

#' Calculate the Local Moran's I Statistic

#' @param x A numeric vector.
#' @inheritParams recreate_listw
#' @param ... See `?spdep::localmoran()` for more options.
#' @importFrom spdep localmoran
#' @family stats
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


#' Categorize LISA
#'
#' @param x Numeric vector.
#' @param x_lag The spatial lag of x as calculated by `st_lag()`.
#' @param scale Whether or not to standardize `x`. Defaults to `TRUE`.
#' @family other
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


