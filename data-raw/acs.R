## code to prepare `acs` dataset goes here


library(sf)
library(tidyverse)

acs <- select(uitk::acs_raw,
              fips = ct_id_10, med_house_income,
              by_pub_trans, bach) %>%
  mutate(fips = as.character(fips),
         across(.cols = c(med_house_income, by_pub_trans, bach),
                ~replace_na(.x, median(.x, na.rm = TRUE))))


acs <- left_join(uitk::suffolk_county, acs, by = "fips") %>%
  select(-geometry, everything())


usethis::use_data(acs, overwrite = TRUE)

