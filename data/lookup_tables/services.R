lookup_tbl <- tibble::tribble(
  ~ServiceCode, ~ServiceDescription,
  "2", "Community service/service learning(CSL)",
  "7", "Criminal justice /legal services",
  "5", "Education",
  "6", "Employment and/or training services",
  "14",  "Health/medical care",
  "26",  "Home-based Services",
  "8",  "Life skills training",
  "10",  "Parenting education for youth with children",
  "27",  "Post-natal newborn care (wellness exams; immunizations)",
  "12",  "Post-natal care for mother",
  "13",  "Pre-natal care",
  "28",  "STD Testing",
  "29",  "Street-based Services",
  "17",  "Substance use disorder treatment",
  "18",  "Substance Use disorder Ed/Prevention"
) |>
  dplyr::arrange(as.numeric(ServiceCode))

source <- list(
  URL = "https://files.hudexchange.info/resources/documents/FY-2022-HMIS-Data-Dictionary.pdf",
  Index = "R14 RHY Service Connections"
)

services_lookup <- list(
  table = lookup_tbl,
  source = source
)

