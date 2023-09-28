# FY2024 HMIS CSV Format Specifications - Version 1.2
# Appendix B – Lists
# 1.6 RaceGenderNone
RaceGenderNoneCodes <- tibble::tribble(
  ~Code, ~Description,
  8L, "Client doesn't know",
  9L, "Client prefers not to answer",
  99L, "Data not collected"
)

usethis::use_data(RaceGenderNoneCodes, overwrite = TRUE)
