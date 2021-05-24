#' Calculate neighbor cardinalities
#'
#' @param neighbors A neighbor list object as created by `st_neighbors()`.
#' @details See `?spdep::card()` for more.
#' @export
st_cardinalties <- function(neighbors) {
  class(neighbors) <- "nb"
  card(neighbors)
}
