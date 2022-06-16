## code to prepare `HealthAndDV` dataset goes here

# R7-R10 Health and Domestic Violence Status (page 62 - 64) in Data Dictionary
HealthAndDVCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Excellent",
  2L, "Very good",
  3L, "Good",
  4L, "Fair",
  5L, "Poor",
  8L, "Client doesn’t know",
  9L, "Client refused",
  99L, "Data not collected"
)


usethis::use_data(
  HealthAndDVCodes,
  overwrite = TRUE
)
