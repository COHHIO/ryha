#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Use this for testing
  # dm <- readRDS("db_data/db_data.rds")

  # Establish connection to PostgreSQL database
  con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv("AWS_POSTGRES_DBNAME"),
    host = Sys.getenv("AWS_POSTGRES_HOST"),
    port = Sys.getenv("AWS_POSTGRES_PORT"),
    user = Sys.getenv("AWS_POSTGRES_USER"),
    password = Sys.getenv("AWS_POSTGRES_PWD")
  )

  # Create dm object. This is run once per session
  dm <- create_dm(conn = con)

  # Create a reactiveValues list to hold summary statistics
  # rctv <- shiny::reactiveValues(
  #   n_youth_total
  #
  # )

  # Get filtered dm
  dm_filtered <- mod_filters_server("filters_1", dm)

  mod_client_server("client_1", dm_filtered)

  mod_living_server("living_1", dm_filtered)

  mod_disabilities_server("disabilities_1", dm_filtered)

  mod_employment_server("employment_1", dm_filtered)

  mod_education_server("education_1", dm_filtered)

  mod_health_server("health_1")

  # mod_exits_server("exits_1")

  mod_upload_server("upload_1", conn = con)

}
