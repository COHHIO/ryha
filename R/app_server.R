#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # w$show()

  # Use this for testing
  dm <- readRDS("db_data/db_data.rds")

  # Create dm object. This is run once per session
  # dm <- create_dm()

  # Create a reactiveValues list to hold summary statistics
  # rctv <- shiny::reactiveValues(
  #   n_youth_total
  #
  # )

  # Get filtered dm
  clients_filtered <- mod_filters_server(
    id = "filters_1",
    dm = dm
  )

  # w$hide()

  # mod_client_server("client_1", clients_filtered)

  mod_living_server(
    id = "living_1",
    living_data = dm$current_living_situation,
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

  # mod_education_server("education_1", clients_filtered)

  mod_health_server(
    id = "health_1",
    health_data = dm$health,
    clients_filtered = clients_filtered
  )

  # mod_exits_server("exits_1")

  mod_upload_server("upload_1")

}
