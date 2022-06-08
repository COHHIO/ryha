


# "https://files.hudexchange.info/resources/documents/FY-2022-HMIS-Data-Dictionary.pdf",
# "R14 RHY Service Connections"

ServiceCodes <- tibble::tribble(
  ~Code, ~Description,
  2L, "Community service/service learning(CSL)",
  7L, "Criminal justice /legal services",
  5L, "Education",
  6L, "Employment and/or training services",
  14L, "Health/medical care",
  26L, "Home-based Services",
  8L,  "Life skills training",
  10L, "Parenting education for youth with children",
  27L, "Post-natal newborn care (wellness exams; immunizations)",
  12L, "Post-natal care for mother",
  13L, "Pre-natal care",
  28L, "STD Testing",
  29L, "Street-based Services",
  17L, "Substance use disorder treatment",
  18L, "Substance Use disorder Ed/Prevention"
)


usethis::use_data(ServiceCodes, overwrite = TRUE)
