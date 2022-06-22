#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # data <- # connect to AWS PostgreSQL database

  # dm <- # create relational data model with {dm}

  data <- shiny::reactiveValues(
    submission = arrow::open_dataset("data_lake/submission/"),
    program = arrow::open_dataset("data_lake/program/"),
    client = arrow::open_dataset("data_lake/client/"),
    ethnicity = arrow::open_dataset("data_lake/ethnicity/"),
    gender = arrow::open_dataset("data_lake/gender/"),
    military = arrow::open_dataset("data_lake/military/")
  )

  mod_gender_server(
    "gender_1",
    data = data
  )

}
