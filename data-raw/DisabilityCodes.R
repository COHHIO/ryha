## code to prepare `Disabilities` dataset goes here

# Project information 2.02 section in Data Dictionary
DisabilityTypeCodes <- tibble::tribble(
  ~Code, ~Description,
  5L, "RETIRED",
  6L, "Services Only",
  7L, "Other",
  8L, "Safe Haven",
  9L, "PH – Housing Only",
  10L, "PH – Housing with Services (no disability required for entry)"
)




usethis::use_data(
  DisabilityTypeCodes,
  overwrite = TRUE
)
