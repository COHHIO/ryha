#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Use this for testing
  # dm <- readRDS("db_data/dm.rds")

  # Create dm object. This is run once per session
  dm <- create_dm()

  # Get filtered dm
  clients_filtered <- mod_filters_server(
    id = "filters_1",
    dm = dm
  )

  mod_overview_server(
    id = "overview_1",
    client_data = dm$client,
    gender_data = dm$gender,
    enrollment_data = dm$enrollment,
    ethnicity_data = dm$ethnicity,
    clients_filtered = clients_filtered
  )

  mod_disabilities_server(
    id = "disabilities_1",
    disabilities_data = dm$disabilities,
    clients_filtered = clients_filtered
  )

  mod_employment_server(
    id = "employment_1",
    employment_data = dm$employment,
    clients_filtered = clients_filtered
  )

  mod_education_server(
    id = "education_1",
    education_data = dm$education,
    clients_filtered = clients_filtered
  )

  mod_health_server(
    id = "health_1",
    health_data = dm$health,
    counseling_data = dm$exit,
    clients_filtered = clients_filtered
  )

  mod_domestic_violence_server(
    id = "domestic_violence_1",
    domestic_violence_data = dm$domestic_violence,
    clients_filtered = clients_filtered
  )

  mod_income_benefits_server(
    id = "income_benefits_1",
    income_data = dm$income,
    benefits_data = dm$benefits,
    clients_filtered = clients_filtered
  )

  mod_services_server(
    id = "services_1",
    services_data = dm$services,
    referral_data = dm$enrollment,
    clients_filtered = clients_filtered
  )

  mod_trafficking_server(
    id = "trafficking_1",
    trafficking_data = dm$exit,
    clients_filtered = clients_filtered
  )

  mod_living_situation_server(
    id = "living_situation_1",
    enrollment_data = dm$enrollment,
    exit_data = dm$exit,
    clients_filtered = clients_filtered
  )

  mod_parenting_server(
    id = "parenting_1",
    health_data = dm$health,
    enrollment_data = dm$enrollment,
    clients_filtered = clients_filtered
  )

  mod_upload_server(
    id = "upload_1"
  )

}
