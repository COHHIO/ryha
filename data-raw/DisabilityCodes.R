##what is the true index to be used for disabilities.csv ??

## code to prepare `Disabilities` dataset goes here

# Project information Appendix B 1.3 section in Data Dictionary
DisabilityTypeCodes <- tibble::tribble(
  ~Code, ~Description,
  5L, "Physical Disability",
  6L, "Developmental Disability",
  7L, "Chronic Health Condition",
  8L, "HIV/AIDS",
  9L, "Mental Health Disorder",
  10L, "Substance Use Disorder"
)




usethis::use_data(
  DisabilityTypeCodes,
  overwrite = TRUE
)
