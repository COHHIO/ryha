#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  client <- reactive(read_client())

  mod_overview_server(
    id = "overview_1",
    data = client
  )

  mod_bar_chart_server(
    id = "bar_chart_1",
    data = client
  )



}
