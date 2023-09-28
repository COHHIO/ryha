# FY2024 HMIS CSV Format Specifications - Version 1.2
# Appendix B – Lists
# 3.12.1 Living Situation Option List

LivingCodes <- tibble::tribble(
  ~Code, ~Description, ~ExitCategory,
  116L, "Place not meant for habitation (e.g., a vehicle, an abandoned building, bus/train/subway station/airport or anywhere outside)", "Homeless",
  101L, "Emergency shelter, including hotel or motel paid for with emergency shelter voucher, Host Home shelter", "Homeless",
  118L, "Safe Haven", "Homeless",
  215L, "Foster care home or foster care group home", "Institutional",
  206L, "Hospital or other residential non-psychiatric medical facility", "Institutional",
  207L, "Jail, prison or juvenile detention facility", "Institutional",
  225L, "Long-term care facility or nursing home", "Institutional",
  204L, "Psychiatric hospital or other psychiatric facility", "Institutional",
  205L, "Substance abuse treatment facility or detox center", "Institutional",
  302L, "Transitional housing for homeless persons (including homeless youth)", "Temporary",
  329L, "Residential project or halfway house with no homeless criteria", "Temporary",
  314L, "Hotel or motel paid for without emergency shelter voucher", "Temporary",
  332L, "Host Home (non-crisis)", "Temporary",
  312L, "Staying or living with family, temporary tenure (e.g. room, apartment or house)", "Temporary",
  313L, "Staying or living with friends, temporary tenure (e.g. room apartment or house)", "Temporary",
  327L, "Moved from one HOPWA funded project to HOPWA TH", "Temporary",
  336L, "Staying or living in a friend's room, apartment, or house", "Temporary",
  335L, "Staying or living in a family member's room, apartment, or house", "Temporary",
  422L, "Staying or living with family, permanent tenure", "Permanent",
  423L, "Staying or living with friends, permanent tenure", "Permanent",
  426L, "Moved from one HOPWA funded project to HOPWA PH", "Permanent",
  410L, "Rental by client, no ongoing housing subsidy", "Permanent",
  435L, "Rental by client, with ongoing housing subsidy", "Permanent",
  421L, "Owned by client, with ongoing housing subsidy", "Permanent",
  411L, "Owned by client, no ongoing housing subsidy", "Permanent",
  30L, "No exit interview completed", "Other",
  17L, "Other", "Other",
  24, "Deceased", "Other",
  37L, "Worker unable to determine", "Other",
  8L, "Client doesn't know", "Other",
  9L, "Client prefers not to answer", "Other",
  99L, "Data not collected", "Other"
)

usethis::use_data(LivingCodes, overwrite = TRUE)
