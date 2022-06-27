## code to prepare `LIVING` dataset goes here

#Appendix A - Living situation option list (LSOL) in Data Dictionary

LSOL_SituationsCodes <- tibble::tribble(
  ~Code, ~Description,

#Homeless Situations information Appendix A in Data Dictionary
# LSOL_Homeless_SituationsCodes <- tibble::tribble(
#   ~Code, ~Description,
  1L, "Emergency shelter, including hotel or motel paid for with emergency shelter
  voucher, or RHY-funded Host Home shelter",
  16L, "Place not meant for habitation (e.g., a vehicle, an abandoned building,
                                       bus/train/subway station/airport or anywhere outside)",
  18L, "Safe Haven",
  # )


#Institutional Situations information Appendix A in Data Dictionary
# LSOL_Institutional_SituationsCodes <- tibble::tribble(
#   ~Code, ~Description,
  4L, "Psychiatric hospital or other psychiatric facility",
  5L, "Substance abuse treatment facility or detox center",
  6L, "Hospital or other residential non-psychiatric medical facility",
  7L, "Jail, prison, or juvenile detention facility",
  15L, "Foster care home or foster care group home",
  25L, "Long-term care facility or nursing home",
# )

#Temporary and Permanent Housing Situations information Appendix A in Data Dictionary
# LSOL_Temp_Perm_SituationsCodes <- tibble::tribble(
#   ~Code, ~Description,
  2L, "Transitional housing for homeless persons (including homeless youth)",
  3L, "Permanent housing (other than RRH) for formerly homeless persons",
  10L, "Rental by client, no ongoing housing subsidy",
  11L, "Owned by client, no ongoing housing subsidy",
  12L, "Staying or living with family, temporary tenure (e.g. room, apartment, or house)",
  13L, "Staying or living with friends, temporary tenure (e.g. room, apartment, or house)",
  14L, "Hotel or motel paid for without emergency shelter voucher",
  19L, "Rental by client, with VASH housing subsidy",
  20L, "Rental by client, with other ongoing housing subsidy",
  21L, "Owned by client, with ongoing housing subsidy",
  22L, "Staying or living with family, permanent tenure",
  23L, "Staying or living with friends, permanent tenure",
  26L, "Moved from one HOPWA funded project to HOPWA PH",
  27L, "Moved from one HOPWA funded project to HOPWA TH",
  28L, "Rental by client, with GPD TIP housing subsidy",
  29L, "Residential project or halfway house with no homeless criteria",
  31L, "Rental by client, with RRH or equivalent subsidy",
  32L, "Host Home (non-crisis)",
  33L, "Rental by client, with HCV voucher (tenant or project based)",
  34L, "Rental by client in a public housing unit",
  35L, "Staying or living in a family member’s room, apartment, or house",
  36L, "Staying or living in a friend’s room, apartment, or house",
# )


#Other Living Situations information Appendix A in Data Dictionary
# LSOL_Other_SituationsCodes <- tibble::tribble(
#   ~Code, ~Description,
  30L, "No exit interview completed",
  17L, "Other",
  24L, "Deceased",
  37L, "Worker unable to determine",
  8L, "Client doesn’t know",
  9L, "Client refused",
  99L, "Data not collected"
)



usethis::use_data(
  LSOL_SituationsCodes,
  # LSOL_Homeless_SituationsCodes,
  # LSOL_Temp_Perm_SituationsCodes,
  # LSOL_Other_SituationsCodes,
  overwrite = TRUE
)
