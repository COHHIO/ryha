## code to prepare `Project` dataset goes here

# Project Type information 2.02 in Data Dictionary
ProjectTypeCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Emergency Shelter",
  2L, "Transitional Housing",
  3L, "PH - Permanent Supportive Housing (disability required for entry)",
  4L, "Street Outreach",
  5L, "RETIRED",
  6L, "Services Only",
  7L, "Other",
  8L, "Safe Haven",
  9L, "PH – Housing Only",
  10L, "PH – Housing with Services (no disability required for entry)",
  11L, "Day Shelter",
  12L, "Homelessness Prevention",
  13L, "PH - Rapid Re-Housing",
  14L, "Coordinated Entry"
)


usethis::use_data(
  ProjectTypeCodes,
  overwrite = TRUE
)
