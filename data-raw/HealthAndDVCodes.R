## code to prepare `HealthAndDV` data set goes here

# R7-R10 Health and Domestic Violence Status (page 62 - 64) in Data Dictionary
HealthStatusCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Excellent",
  2L, "Very good",
  3L, "Good",
  4L, "Fair",
  5L, "Poor",
  8L, "Client doesn’t know",
  9L, "Client refused",
  99L, "Data not collected"
)


# Domestic Violence Status 4.11 section in Data Dictionary
#if yes for domestic violence victim/survivor
WhenDVOccurredCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Within the past three months",
  2L, "Three to six months ago (excluding six months exactly)",
  3L, "Six months to one year ago (excluding one year exactly)",
  4L, "One year ago, or more",
  8L, "Client doesn’t know",
  9L, "Client refused",
  99L, "Data not collected"
)

usethis::use_data(
  HealthStatusCodes,
  WhenDVOccurredCodes,
  overwrite = TRUE
)
