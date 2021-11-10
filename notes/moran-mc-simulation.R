# spatial simulation
library(sfweight)
library(tidyverse)

nb <- acs %>%
  mutate(nb = st_contiguity(geometry),
         wt = st_weights(nb))

# Observed distribution

p0 <- ggplot(nb, aes(fill = med_house_income)) +
  geom_sf(color = "black", lwd = 0.2)

# randomized once
p1 <- ggplot(nb, aes(fill = sample(med_house_income))) +
  geom_sf(color = "black", lwd = 0.2)

# twice
p2 <- ggplot(nb, aes(fill = sample(med_house_income))) +
  geom_sf(color = "black", lwd = 0.2)

# thrice
p3 <- ggplot(nb, aes(fill = sample(med_house_income))) +
  geom_sf(color = "black", lwd = 0.2)

library(patchwork)

p0 / p1 / p2 / p3

# Calculating pseudo p value
# First we calculate the global Moran I
mi <- moran_test(nb, med_house_income, nb, wt)

# Now we simulate this for "reference" distributions
# or randomly assigned values to space

nb %>%
  mutate(ref = sample(med_house_income)) %>%
  moran_test(ref, nb, wt)

# The result is insignificant.
# This lends itself to the belief that our observed
# values are _not_ spatially random.

# This is the idea behind the pseudo p value for global
# spatial autocorrelation. The formula for which is p = R + 1 / M + 1.
# p = pseudo p value
# R = number of times I is significant
# M = number of simulations


# Let's do some monte-carlo simulations!
# These will be rather crude but just enough to get the point across!
# First step with doing simulations is to make a function
# functions are easy to repeat
# we will iterate over the function a bunch of times and collect our outputs.
sim_var <- function(df, var, nb, wt) {
  res <- df %>%
    mutate(ref = sample({{ var }})) %>%
    moran_test(ref, {{ nb }}, {{ wt }}, randomisation = FALSE)

  ifelse(res$p.value <= 0.05, 1, 0)
}


# Try out the function once
sim_var(nb, med_house_income, nb, wt)

# draw 9 samples
sims_9 <- map_dbl(1:9, ~sim_var(nb, med_house_income, nb, wt))

# calculate by hand
(sum(sims_9) + 1) / (length(sims_9) + 1)

# create function
pseudo_p <- function(x) {
  (sum(x) + 1) / (length(x) + 1)
}

# use function
pseudo_p(sims_9)

# take 99 samples
sims_99 <- map_dbl(1:99L, ~sim_var(nb, med_house_income, nb, wt))

# pseudo p has decreased to a point of "significance"
pseudo_p(sims_99)

# check with 499 simulations
sims_499 <- map_dbl(1:499, ~sim_var(nb, med_house_income, nb, wt))

pseudo_p(sims_499)

# we can look at the sampling distribution of our p values
x <- map(1:100, ~{
  pseudo_p(map_dbl(1:99, ~sim_var(nb, med_house_income, nb, wt)))
})

plot(density(unlist(x)))
hist(unlist(x))
# As we repeat this more and more we get a better sensee of the reference distribution


# There are functions that provide a more robust and effective calculation of this measure
# that should be used
mmc <- moran_mc(nb, med_house_income, nb, wt)
mmc
