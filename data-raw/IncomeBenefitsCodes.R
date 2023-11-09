## code to prepare `IncomeBenefits` dataset goes here

# Health Insurance section 4.04 page 40 in Data Dictionary
ReasonNotInsuredCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Applied; decision pending",
  2L, "Applied; client not eligible",
  3L, "Client did not apply",
  4L, "Insurance type N/A for this client",
  8L, "Client doesn't know",
  9L, "Client prefers not to answer",
  99L, "Data not collected"
)

usethis::use_data(
  ReasonNotInsuredCodes,
  overwrite = TRUE
)
