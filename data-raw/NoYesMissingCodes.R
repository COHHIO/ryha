# FY2024 HMIS CSV Format Specifications - Version 1.2
# Appendix B – Lists
# 1.7 No/Yes/Missing
NoYesMissingCodes <- tibble::tribble(
  ~Code, ~Description,
  0L, "No",
  1L, "Yes",
  99L, "Data not collected"
)

usethis::use_data(NoYesMissingCodes, overwrite = TRUE)
