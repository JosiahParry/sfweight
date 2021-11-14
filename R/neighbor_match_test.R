#' @title Local Neighbor Match Test
#'
#' @description Implements the Local Neighbor Match Test as described in _Tobler's Law in a Multivariate World_ (Anselin and Li, 2020).
#'
#' @export
#'
#'@examples
#' acs %>%
#'   mutate(nb = st_knn(geometry, k = 10),
#'          nmt = nb_match_test(.cols = list(med_house_income, bach),
#'                              nb = nb,
#'                              k = 10))
#'
#' acs %>%
#'   mutate(nb = st_knn(geometry, k = 10)) %>%
#'   nb_match_test(.cols = c(med_house_income, bach),
#'                 nb = nb,
#'                 k = 10)
#'

nb_match_test <- function(x, ...) {
  UseMethod("nb_match_test")
}

# Personal thought, can we extend this concept a bit more to use any form of neighbors matrix? For example contiguities? Then the calculation of p values are a bit different. Your contiguities might have different numbers. Say your k is set to 10 but card(nb) = 5.
# what is the probability of sampling 10 neighbors and of those 10 5 of them are same as your existing neighbors.

#' @param .cols A list of numeric vectors. Intended to be used inside of a `dplyr::mutate()` call. In the case of a data.frame, the columns to be used in calculating the distance matrix using tidy selection.
#' @method nb_match_test default
#' @describeIn nb_match_test Conduct a neighbor match test within a mutate call.
#' @export
nb_match_test.default <- function(.cols, nb, k = 10, method = "euclidean", scale = TRUE, p = 2) {

  d <- cast_dist_list(.cols = .cols, method = method, scale = scale, p = p)

  compute_nmt(d, k = k, nb = nb)

}

#' @param .data A data frame.
#' @param nb The unquoted name of your knn neighbor list.
#' @param k The number of neighbors to identify. Should be the same `k` used in `st_knn()`.
#' @param method The distance measure to be used by `stats::dist()`.
#' @param scale Whether or not to standardize columns prior to calculation. It is strongly recommended to do so.
#' @param p The power of the Minkowski distance.
#'
#' @method nb_match_test data.frame
#' @describeIn nb_match_test Conduct a neighbor match test with a data frame.
#' @export
nb_match_test.data.frame <- function(.data, .cols, nb, k = 10,
                                           method = "euclidean", scale = TRUE, p = 2) {

  d <- cast_dist_df(.data, .cols = {{.cols}}, method, scale, p)
  nbs <- dplyr::pull(.data, {{ nb }})
  compute_nmt(d, k = k, nb = nbs)

}


# Casting to distance matrix ----------------------------------------------
# Help functions
# Two separate functions for casting to distance matrix
cast_dist_df <- function(.data, .cols, method, scale, p) {

  if (any(class(.data) == "sf")) .data <- tibble::as_tibble(.data)

  df <- dplyr::select(.data, {{ .cols }})

  if (scale) df <- dplyr::mutate(df, dplyr::across(.cols = everything(), .fns = base::scale))

  dist(df, method = method, p = p)

}

cast_dist_list <- function(.cols, method, scale, p) {
  m <- Reduce(cbind, .cols)
  if (scale) m <- scale(m)

  dist(m, method = method, p = p)

}

# Calculate Neighbor Match Test -------------------------------------------
# one function to do the computation that will be shared among s3 methods for nmt
compute_nmt <- function(d, k, nb) {

  kd <- dbscan::kNN(d, k = k)

  # find neighbor ids
  knn_attr <- dbscan::adjacencylist(kd)

  # find matches
  matches <- purrr::map2(knn_attr, nb, base::intersect)

  N <- attr(d, "Size") - 1
  v <- lengths(matches)
  k <- 10

  p_vals <- choose(k, v) * choose(N - k, k - v) / choose(N, k)


  tibble::tibble(
    attr_nb = knn_attr,
    nb_matches = matches,
    n_shared = lengths(matches),
    p_value = p_vals
  )
}

