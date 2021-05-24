#' Calculate spatial weights
#' @param neighbors A neighbor list object as created by `st_neighbors()`.
#' @param style Default `"W"` for row standardized weights.
#' @importFrom spdep nb2listw
#' @export
st_weights <- function(neighbors, style = "W", allow_zero = NULL, ...) {

  class(neighbors) <- "nb"

  listw <- nb2listw(neighbors, style = style, zero.policy = allow_zero, ...)

  listw[["weights"]]
}


#' Calculate Inverse Distance Bands
#'
#' @param x Spatial points. Typically the `geometry` column of an sf object.
#' @param knn A knn neighbor list as created from `st_knn()`.
#'
#' @details See implementation details [here](https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html#kernal-weights).
#'
#' @importFrom spdep dnearneigh nbdists
#' @export
st_inverse_weights <- function(x, knn) {
  # As implemented by Luc Anselin
  # https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html#inverse-distance-weights
  class(knn) <- "nb"

  threshold <- max(unlist(nbdists(knn, x)))

  dist_band <- dnearneigh(x, 0, threshold)

  distances <- nbdists(dist_band, x)

  lapply(distances, function(x) (1/(x/100)))

}


#' Calculate Kernal Weights
#'
#'
#' @inheritParams st_inverse_weights
#' @param kernal One of "uniform", "gaussian",  "triangular", "epanechnikov", or "quartic".
#' @importFrom spdep nbdists dnearneigh include.self
#' @export
st_kernal_weight <- function(x, knn, kernal = "uniform") {

  class(knn) <- "nb"

  threshold <- max(unlist(nbdists(knn, x)))

  kernal_nb <- dnearneigh(x, 0, threshold)

  kernal_nb <- include.self(kernal_nb)

  kernal_dists <- nbdists(kernal_nb, x)

  lapply(kernal_dists, kernals[[kernal]], threshold)


}
