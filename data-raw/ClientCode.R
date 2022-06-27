# These are the "Client" codes used in the HMIS data


# Ethnicity codes from section 3.05 in Data Dictionary
EthnicityCodes <- tibble::tribble(
  ~Code, ~Description,
  0L, "No",
  1L, "Yes",
  8L, "Client doesn't know",
  9L, "Client refused",
  99L, "Data not collected"
)


usethis::use_data(GeneralCodes, overwrite = TRUE)
