


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

#based on 'HMIS CSV FORMAT Specifications FY2022 – May 2021' guide pdf file, I had
#followed Services.csv informaiton in page 29 and followed 'record type" = 142

# Type provided information R14.2 RHYServices (page 49) in Data Dictionary
# used in Services.csv when RecordType = 142 (RHY service).

RecordTypeRHYServicesCodes <- tibble::tribble(
  ~Code, ~Description,
  2L, "Community service/service learning (CSL)",
  5L, "Education",
  6L, "Employment and/or training services",
  7L, "Criminal justice /legal services",
  8L, "Life skills training",
  10L, "Parenting education for youth with children",
  12L, "Post-natal care for mother",
  13L, "Pre-natal care",
  14L, "Health/medical care",
  17L, "Substance use disorder treatment",
  18L, "Substance use disorder Ed/Prevention Services",
  26L, "Home-based Services",
  27L, "Post-natal newborn care (wellness exams; immunizations)",
  28L, "STD Testing",
  29L, "Street-based Services"
)
usethis::use_data(ServiceCodes,
                  RecordTypeRHYServicesCodes,
                  overwrite = TRUE)
