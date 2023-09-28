# FY2024 HMIS CSV Format Specifications - Version 1.2
# Appendix B – Lists
# 1.8 No/Yes/Reasons for Missing Data
NoYesReasonsForMissingDataCodes <- tibble::tribble(
  ~Code, ~Description,
  0L, "No",
  1L, "Yes",
  8L, "Client doesn't know",
  9L, "Client prefers not to answer",
  99L, "Data not collected"
)

usethis::use_data(NoYesReasonsForMissingDataCodes, overwrite = TRUE)
