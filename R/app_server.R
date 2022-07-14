#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Create dm object. This is run once per session
  dm <- create_dm()

  # Create a reactiveValues list to hold summary statistics
  # rctv <- shiny::reactiveValues(
  #   n_youth_total
  #
  # )

  # Get filtered dm
  dm_filtered <- mod_filters_server("filters_1", dm)

  mod_client_server("client_1", dm_filtered)

  mod_living_server("living_1", dm_filtered)

}
