


RelationshipToHoHCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Self (head of household)",
  2L, "Head of household’s child",
  3L, "Head of household’s spouse or partner",
  4L, "Head of household’s other relation member",
  5L, "Other: non-relation member",
  99L, "Data not collected"
)

LengthOfStayCodes <- tibble::tribble(
  ~Code, ~Description,
  2L, "One week or more, but less than one month",
  3L, "One month or more, but less than 90 days",
  4L, "90 days or more but less than one year",
  5L, "One year or longer",
  8L, "Client doesn’t know",
  9L, "Client refused",
  10L, "One night or less",
  11L, "Two to six nights",
  99L, "Data not collected"
)

TimesHomelessPastThreeYearsCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "One time",
  2L, "Two times",
  3L, "Three times",
  4L, "Four or more times",
  8L, "Client doesn’t know",
  9L, "Client refused",
  99L, "Data not collected"
)

MonthsHomelessPastThreeYearsCodes <- tibble::tribble(
  ~Code, ~Description,
  8L, "Client doesn’t know",
  9L, "Client refused",
  99L, "Data not collected",
  101L, "1 month",
  102L, "2 months",
  103L, "3 months",
  104L, "4 months",
  105L, "5 months",
  106L, "6 months",
  107L, "7 months",
  108L, "8 months",
  109L, "9 months",
  110L, "10 months",
  111L, "11 months",
  112L, "12 months",
  113L, "More than 12 months"
)

ReferralSourceCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Self-Referral",
  2L, "Individual: Parent/Guardian/Relative/Friend/Foster Parent/Other Individual",
  7L, "Outreach Project",
  11L, "Temporary Shelter",
  18L, "Residential Project",
  28L, "Hotline",
  30L, "Child Welfare/CPS",
  34L, "Juvenile Justice",
  35L, "Law Enforcement/ Police",
  37L, "Mental Hospital",
  38L, "School",
  39L, "Other Organization",
  8L, "Client doesn’t know",
  9L, "Client refused",
  99L, "Data not collected"
)

SexualOrientationCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Heterosexual",
  2L, "Gay",
  3L, "Lesbian",
  4L, "Bisexual",
  5L, "Questioning / unsure",
  6L, "Other",
  8L, "Client doesn’t know",
  9L, "Client refused",
  99L, "Data not collected"
)

RHYNumberOfYearsCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Less than one year",
  2L, "1 to 2 years",
  3L, "3 to 5 or more years"
)

usethis::use_data(
  RelationshipToHoHCodes,
  LengthOfStayCodes,
  TimesHomelessPastThreeYearsCodes,
  MonthsHomelessPastThreeYearsCodes,
  ReferralSourceCodes,
  SexualOrientationCodes,
  RHYNumberOfYearsCodes,
  overwrite = TRUE
)
