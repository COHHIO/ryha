## code to prepare `geocodes` dataset goes here
geocodes <- arrow::read_feather("data-raw/geocodes.feather") |>
  janitor::clean_names() |>
  dplyr::rename(geocode = geographic_code)

usethis::use_data(geocodes, overwrite = TRUE)
