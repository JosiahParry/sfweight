library(spdep)
library(sfweight)
library(tidyverse)

col.gal.nb <- read.gal(system.file("weights/columbus.gal", package="spData")[1])

# https://dces.wisc.edu/wp-content/uploads/sites/128/2013/08/W14_Anselin2007.pdf

unclass(col.gal.nb)

colum <- columbus %>%
  as_tibble() %>%
  st_as_sf(coords = c("X", "Y")) %>%
  mutate(nb = unclass(col.gal.nb),
         wt = st_weights(nb))

# morans I
crime_i <- colum %>%
  moran_test(CRIME, nb, wt,
             alternative = "two.sided",
             randomisation = FALSE)

# Moran permutation
colum_lw <- recreate_listw(colum$nb, colum$wt)

colum_perm <- moran.mc(colum$CRIME, colum_lw, nsim = 99)

ggplot() +
  geom_density(aes(x = colum_perm$res)) +
  geom_histogram(aes(x = colum_perm$res), alpha = 1/2, bins = 10) +
  geom_vline(xintercept = colum_perm$statistic)


# 3.4.1

colum %>%
  mutate(across(c(CRIME, INC, HOVAL), st_lag, neighbors = nb, weights = wt))

# 4.6.2
#' The expected value of Morans I is -1 / n - 1 approaches -1
#' "Since the mean and variance of the statistic under the null depend\
#' solely on the weights matrix, and not on the actual variables under consideration, they must only be computed once."
