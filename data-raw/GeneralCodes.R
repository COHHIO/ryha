


# These are the 5 "general" codes that many of the variables in the HMIS data
#

GeneralCodes <- tibble::tribble(
  ~Code, ~Description,
  0L, "No",
  1L, "Yes",
  8L, "Client doesn't know",
  9L, "Client refused",
  99L, "Data not collected"
)


usethis::use_data(GeneralCodes, overwrite = TRUE)
