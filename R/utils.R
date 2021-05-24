`%||%` <- function (x, y) {
  if (is_null(x))
    y
  else x
}




#' Create a listw object from a neighbors and weight list
#'
#' @param neighbors A neighbor list object as created by `st_neighbors()`.
#' @param weights A weights list as created by `st_weights()`.
#' @keywords internal
recreate_listw <- function(neighbors, weights) {
  which_style <- c(attr(weights, "W") %||% NA,
                   attr(weights, "B") %||% NA,
                   attr(weights, "C") %||% NA,
                   attr(weights, "U") %||% NA,
                   attr(weights, "minmax") %||% NA,
                   attr(weights, "S") %||% NA)

  possible_styles <- c("W", "B", "C", "U", "minmax", "S")

  class(neighbors) <- "nb"

  listw <- list(style = possible_styles[!is.na(which_style)],
                neighbours = neighbors,
                weights = weights)

  class(listw) <- "listw"

  listw
}




kernels <- list(
  uniform = function(x, ...) x * 0 + .5,
  triangular = function(x, thresh) 1 - abs(x / thresh),
  epanechnikov = function(x, thresh) .75 * (1- (x / thresh)^2),
  quartic = function(x, thresh) (15/16)*(1-(x/thresh)^2)^2,
  gaussian =  function(x, thresh) sqrt(2*pi)*exp((-(x/thresh)^2)/2)
)


