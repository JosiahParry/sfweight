#' Calculate K-Nearest Neighbors
#'
#' @param x An sf or sfc object.
#' @importFrom sf st_centroid st_geometry
#' @importFrom spdep knn2nb knearneigh
#' @export
st_knn <- function(x, k = 1, ...) {

  #sf_col <- attr(x, "sf_column")

  polygon_check <- any(class(x) %in% c("sfc_MULTIPOLYGON", "sfc_POLYGON"))

  if (polygon_check) {
    warning("Polygon provided. Using centroid.")
    pnts <- st_centroid(x)
  } else {
    pnts <- x
  }

  ks <- spdep::knearneigh(pnts, k = k, ...)

  unclass(spdep::knn2nb(ks))
}
#
# columbus %>%
#   mutate(knn_nb = st_knn_neighbors(geometry, k = 2),
#          nb = st_neighbors(geometry))
