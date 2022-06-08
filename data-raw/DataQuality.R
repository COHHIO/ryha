## code to prepare `DataQuality` dataset goes here

# Section 3.02 in Data Dictionary
SSNDataQualityCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Full SSN reported",
  2L, "Approximate or partial SSN reported",
  8L, "Client doesn't know",
  9L, "Client refused",
  99L, "Data not collected"
)

# Section 3.02 in Data Dictionary
DOBDataQualityCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Full DOB reported",
  2L, "Approximate or partial DOB reported",
  8L, "Client doesn't know",
  9L, "Client refused",
  99L, "Data not collected"
)

usethis::use_data(
  SSNDataQualityCodes,
  DOBDataQualityCodes,
  overwrite = TRUE
)
