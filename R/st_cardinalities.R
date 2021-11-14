#' Calculate neighbor cardinalities
#'
#' @param nb A neighbor list object as created by `st_neighbors()`.
#' @family other
#' @export
st_cardinalties <- function(nb) {
  # class(neighbors) <- "nb"
  # spdep::card(neighbors)
  lengths(nb)
}
