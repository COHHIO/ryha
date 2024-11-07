## code to prepare `CountyCodes` dataset goes here
CountyCodes <- arrow::read_feather("data-raw/geocodes.feather") |>
  janitor::clean_names() |>
  dplyr::select(geocode = geographic_code, county = county) |> 
  dplyr::arrange(county, geocode)

usethis::use_data(CountyCodes, overwrite = TRUE)
