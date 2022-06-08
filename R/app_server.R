#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # data <- reactive(read_client())

  data <- shiny::reactiveValues(
    submission = arrow::open_dataset("data_lake/submission/"),
    program = arrow::open_dataset("data_lake/program/"),
    client = arrow::open_dataset("data_lake/client/"),
    ethnicity = arrow::open_dataset("data_lake/ethnicity/"),
    gender = arrow::open_dataset("data_lake/gender/"),
    military = arrow::open_dataset("data_lake/military/")
  )

  mod_overview_server(
    id = "overview_1",
    data = data()$client_full
  )

  mod_bar_chart_server(
    id = "bar_chart_1",
    data = data
  )

  mod_upload_server(
    id = "upload_1"
  )

}
