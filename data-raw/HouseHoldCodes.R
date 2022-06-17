## code to prepare `Enrollment` dataset goes here

# Relationship to Head of Household Section 3.15 in Data Dictionary :
HouseHold_relationshipToCodes <- tibble::tribble(
  ~Code, ~Description,
  1L, "Self",
  2L, "Head of Household's child",
  3L, "Head of Household's spouse or partner",
  4L, "Head of household’s other relation member (other relation to head of household)",
  5L, "Other: non-relation member",
)



usethis::use_data(
  HouseHold_relationshipToCodes,
  overwrite = TRUE
)
