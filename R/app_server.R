#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  data <- shiny::reactiveValues(
    con = DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = Sys.getenv("POSTGRES_DBNAME"),
      host = Sys.getenv("POSTGRES_HOST"),
      port = Sys.getenv("POSTGRES_PORT"),
      user = Sys.getenv("POSTGRES_USER"),
      password = Sys.getenv("POSTGRES_PWD")
    )
  )

  shiny::observe({

    data$submission <- DBI::dbReadTable(conn = data$con, name = "submission")
    data$project <- DBI::dbReadTable(conn = data$con, name = "project")
    data$client <- DBI::dbReadTable(conn = data$con, name = "client")

    on.exit(DBI::dbDisconnect(conn = data$con))

  })

  mod_gender_server(
    "gender_1",
    gender_data = data$client |>
      dplyr::select(submission_id, personal_id, female:questioning) |>
      tidyr::pivot_longer(cols = -c(submission_id, personal_id), names_to = "gender") |>
      dplyr::filter(value == "Yes") |>
      dplyr::select(-value)
  )

}
