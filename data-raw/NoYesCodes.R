# FY2024 HMIS CSV Format Specifications - Version 1.2
# Appendix B – Lists
# 1.10 No/Yes
NoYesCodes <- tibble::tribble(
  ~Code, ~Description,
  0L, "No",
  1L, "Yes"
)

usethis::use_data(NoYesCodes, overwrite = TRUE)
