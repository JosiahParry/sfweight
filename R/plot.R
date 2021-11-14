# Plots

#
#' @family other
plot_moran <- function(x, var, nb, wt) {
  df <- mutate(x,
         across({{ var }},
                .fns = ~st_lag(.x, {{ nb }}, {{ wt }}),
                .names = "{.col}_lag"))


}

#
#
# plot_moran(acs_lagged, med_house_income, nb, wt)
#
# categorize_lisa(acs_lagged[[]])
#
#
#
#
#
#
# acs %>%
#   mutate(nb = st_contiguity(geometry),
#          wt = st_weights(nb)) %>%
#   mutate(across(med_house_income, ~st_lag(.x, nb, wt), .names = "{.col}_lag")) ->
#   acs_lagged
#
#
#   glimpse()
#   mutate(inc_lag = st_lag(med_house_income, nb, wts),
#          lisa_group = categorize_lisa(med_house_income, inc_lag)) %>%
#   ggplot(aes(med_house_income, inc_lag, color = lisa_group)) +
#   geom_vline(aes(xintercept = mean(med_house_income)), lty = 2, alpha = 1/3) +
#   geom_hline(aes(yintercept = mean(inc_lag)), lty = 2, alpha = 1/3) +
#   geom_point() +
#   labs(title = "Moran Plot",
#        y = "Med. HH Income Spatial Lag",
#        x = "Median Household Income") +
#   theme_minimal()

# x must be sf
map_lisa <- function(x, var, nb, wt) {

}


# map local neighbor match test
map_match_test <- function() {

}
