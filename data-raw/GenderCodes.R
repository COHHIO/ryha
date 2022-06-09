## code to prepare `Gender` dataset goes here

# Section 3.06 in Data Dictionary
GenderCodes <- tibble::tribble(
  ~Code, ~Description,
  0L, "Female",
  1L, "Male",
  4L, "A gender other than singularly female or male",
  5L, "Transgender",
  6L, "Questioning"
)

usethis::use_data(
  GenderCodes,
  overwrite = TRUE
)
