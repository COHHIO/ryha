#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Create dm object. This is run once per session
  my_dm <- create_dm()

  # Get filtered dm
  my_dm_filtered <- mod_filters_server("filters_1", my_dm)

  mod_client_server("client_1", my_dm, my_dm_filtered)
}
