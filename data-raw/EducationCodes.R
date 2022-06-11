## code to prepare `EmploymentEducation` dataset goes here

# R4 Last Grade Completed (page 60) in Data Dictionary
LastGradeCompletedCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Less than Grade 5",
  2L, "Grades 5-6",
  3L, "Grades 7-8",
  4L, "Grades 9-11",
  5L, "Grades 12 / High school diploma",
  6L, "School program does not have grade levels",
  7L, "GED",
  10L, "Some College",
  11L, "Associate's Degree",
  12L, "Bachelor's Degree",
  13L, "Graduate Degree",
  14L, "Vocational Degree",
  8L, "Client doesn't know",
  9L, "Client refused",
  99L, "Data not collected"
)


# R5 School Status (page 61) in Data Dictionary
SchoolStatusCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Attending school regularly",
  2L, "Attending school irregularly",
  3L, "Graduated from highschool",
  4L, "Obtained GED",
  5L, "Dropped out",
  6L, "Suspended",
  7L, "Expelled",
  8L, "Client doesn't know",
  9L, "Client refused",
  99L, "Data not collected"
)

#general codes for basic questioner
#than yes or no

# R6 YES Employment Status (page 61) in Data Dictionary
Employment_Yes_Status <- tibble::tribble(
  ~Code, ~Description,
  1L, "Full-time",
  2L, "Part-time",
  3L, "Seasonal / sporadic (including day labor)",
  8L, "Client doesn't know",
  9L, "Client refused",
  99L, "Data not collected"
)

# R6 No Employment Status (page 61) in Data Dictionary
Employment_NO_Status <- tibble::tribble(
  ~Code, ~Description,
  1L, "Looking for work",
  2L, "Unable to work",
  3L, "Not looking for work",
  8L, "Client doesn't know",
  9L, "Client refused",
  99L, "Data not collected"
)

usethis::use_data(
  LastGradeCompletedCodes,
  SchoolStatusCodes,
  Employment_Yes_Status,
  Employment_NO_Status,
  overwrite = TRUE
)
