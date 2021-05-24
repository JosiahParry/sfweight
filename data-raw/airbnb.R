library(sf)
library(tidyverse)

airbnb <- uitk::airbnb %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

usethis::use_data(airbnb, overwrite = TRUE)
