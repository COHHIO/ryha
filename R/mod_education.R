#' education UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_education_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' education Server Functions
#'
#' @noRd 
mod_education_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_education_ui("education_1")
    
## To be copied in the server
# mod_education_server("education_1")
