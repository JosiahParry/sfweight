# See https://github.com/r-spatial/spdep/pull/66/files
# library(spdep)
# library(sfweight)
# library(tidyverse)
# acs_lagged <- mutate(acs,
#                      nb = st_contiguity(geometry),
#                      wt = st_weights(nb))
#
# listw <- sfweight:::recreate_listw(acs_lagged$nb, acs_lagged$wt)
#
#
#
# localC <- function(x, listw, ...) {
#   x <- scale(x)
#
#   xij <- sapply(listw$neighbours, FUN = function(listw) x[listw])
#   mapply(function(x, j, wi) sum(wi * (j - x)^2), x, xij, listw$weights)
# }
#
# localC_perm <- function(x, listw, nsim = 499, zero.policy = NULL, spChk = NULL) {
#
#   obs <- localC(x, listw)
#   reps <- replicate(nsim, localC(sample(x), listw))
#
#   pseudo_p <- (rowSums(obs > reps) + 1)/ (nsims + 1)
#
#   data.frame(
#     ci = obs,
#     pseudo_p = pseudo_p
#   )
# }
#
#
# data.frame(
#   ci = obs,
#   pseudo_p = pseudo_p
# ) %>%
#   arrange(pseudo_p)
#
#
#
# nsims <- 999
# obs <- localC(acs$med_house_income, listw)
# reps <- replicate(nsims, localC(sample(acs$med_house_income), listw))
#
# pseudo_p <- (rowSums(obs <= reps) + 1)/ (nsims + 1)
# sort(pseudo_p)
#
# tibble(c = pseudo_p, g = y[,5]) %>%
#   arrange(g) %>%
#   print(n = Inf)
#
# y <- localmoran_perm(acs$med_house_income, listw)
# cor(y[,5], pseudo_p)
