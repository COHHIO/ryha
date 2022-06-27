## code to prepare `Project` dataset goes here

# Referral provided Type information P2 (page 54) in Data Dictionary
ReferralsProvidedCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Community Mental Health",
  2L, "Substance Use Treatment",
  3L, "Primary Health/ Dental Care",
  4L, "Job Training",
  5L, "Educational Services",
  6L, "Housing Services",
  7L, "Permanent Housing",
  8L, "Income Assistance",
  9L, "Employment Assistance",
  10L, "Medical Insurance",
  11L, "Temporary Housing"
)

#Referral Outcome if any type of referral was made infromation P2 (page 54) in Data Dictionary
ReferralsOutcomeCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Successful referral: client accepted",
  2L, "Unsuccessful referral: client rejected",
  3L, "Unsuccessful referral: provider rejected"
)

usethis::use_data(
  ReferralsProvidedCodes,
  ReferralsOutcomeCodes,
  overwrite = TRUE
)
