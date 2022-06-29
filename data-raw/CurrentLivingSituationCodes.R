## code to prepare `CurrentLivingSituationCodes` dataset goes here

CurrentLivingSituationCodes <- tibble::tribble(
  ~Code, ~Description,
  16L, "Place not meant for habitation (e.g., a vehicle, an abandoned building, bus/train/subway station/airport or anywhere outside)",
  1L, "Emergency shelter, including hotel or motel paid for with emergency shelter voucher, or RHY-funded Host Home shelter",
  18L, "Safe Haven",
  15L, "Foster care home or foster care group home",
  6L, "Hospital or other residential non-psychiatric medical facility",
  7L, "Jail, prison or juvenile detention facility",
  25L, "Long-term care facility or nursing home",
  4L, "Psychiatric hospital or other psychiatric facility",
  5L, "Substance abuse treatment facility or deto center",
  29L, "Residential project or halfway house with no homeless criteria",
  14L, "Hotel or motel paid for without emergency shelter voucher",
  2L, "Transitional housing for homeless persons (including homeless youth)",
  32L, "Host Home (non-crisis)",
  13L, "Staying or living with friends, temporary tenure (e.g. room apartment or house)",
  36L, "Staying or living in a friend’s room, apartment or house",
  35L, "Staying or living in a family member’s room, apartment or house",
  28L, "Rental by client, with GPD TIP housing subsidy",
  19L, "Rental by client, with VASH housing subsidy",
  3L, "Permanent housing (other than RRH) for formerly homeless persons",
  31L, "Rental by client, with RRH or equivalent subsidy",
  33L, "Rental by client, with HCV voucher (tenant or project based)",
  34L, "Rental by client in a public housing unit",
  10L, "Rental by client, no ongoing housing subsidy",
  20L, "Rental by client, with other ongoing housing subsidy",
  21L, "Owned by client, with ongoing housing subsidy",
  11L, "Owned by client, no ongoing housing subsidy",
  17L, "Other",
  37L, "Worker unable to determine",
  8L, "Client doesn’t know",
  9L, "Client refused",
  99L, "Data not collected"
)

usethis::use_data(CurrentLivingSituationCodes, overwrite = TRUE)
