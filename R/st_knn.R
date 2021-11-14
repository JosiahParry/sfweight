#' Calculate K-Nearest Neighbors
#'
#'
#' @param x An sf or sfc object.
#' @importFrom sf st_centroid st_geometry
#' @importFrom spdep knn2nb knearneigh
#' @family neighbors
#' @export
st_knn <- function(x, k = 1, ...) {

  #sf_col <- attr(x, "sf_column")

  polygon_check <- any(class(x) %in% c("sfc_MULTIPOLYGON", "sfc_POLYGON"))

  if (polygon_check) {

    cli::cli_alert_info("Polygon provided. Using centroid.")
    pnts <- st_centroid(x)
  } else {
    pnts <- x
  }

  ks <- spdep::knearneigh(pnts, k = k, ...)

  unclass(spdep::knn2nb(ks))
}
