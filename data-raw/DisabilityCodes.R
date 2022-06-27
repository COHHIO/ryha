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

#for disability type 5-9 generalCodes would be used, but for disability type 10: SubstanceUseDisorderCodes would be used
# Substance Use Disorder Codes information 4.10 section in Data Dictionary (older guide)
SubstanceUseDisorderCodes <- tibble::tribble(
  ~Code, ~Description,
  0L, "No",
  1L, "Alcohol use disorder",
  2L, "Drug use disorder",
  3L, "Both alcohol and drug use disorders",
  8L, "Client doesn’t know",
  9L, "Client refused",
  99L, "Data not collected"
)




usethis::use_data(
  DisabilityTypeCodes,
  SubstanceUseDisorderCodes,
  overwrite = TRUE
)
