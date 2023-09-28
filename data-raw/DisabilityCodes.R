


DisabilityTypeCodes <- tibble::tribble(
  ~Code, ~Description,
  5L, "Physical Disability",
  6L, "Developmental Disability",
  7L, "Chronic Health Condition",
  8L, "HIV/AIDS",
  9L, "Mental Health Disorder",
  10L, "Substance Use Disorder"
)

SubstanceUseDisorderCodes <- tibble::tribble(
  ~Code, ~Description,
  0L, "No",
  1L, "Alcohol use disorder",
  2L, "Drug use disorder",
  3L, "Both alcohol and drug use disorders",
  8L, "Client doesn't know",
  9L, "Client refused",
  99L, "Data not collected"
)

usethis::use_data(
  DisabilityTypeCodes,
  SubstanceUseDisorderCodes,
  overwrite = TRUE
)
