#' domestic_violence UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_domestic_violence_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' domestic_violence Server Functions
#'
#' @noRd 
mod_domestic_violence_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_domestic_violence_ui("domestic_violence_1")
    
## To be copied in the server
# mod_domestic_violence_server("domestic_violence_1")
