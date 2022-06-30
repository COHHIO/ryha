#' filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Gender filter
    shiny::checkboxGroupInput(
      inputId = ns("gender"),
      label = "Gender",
      choices = c(
        "Female",
        "Male",
        "No Single Gender",
        "Transgender",
        "Missing Data"
      ),
      selected = c(
        "Female",
        "Male",
        "No Single Gender",
        "Transgender",
        "Missing Data"
      )
    ),

    # Ethnicity filter
    shiny::checkboxGroupInput(
      inputId = ns("ethnicity"),
      label = "Ethnicity",
      choices = c(
        "White",
        "Black and African American",
        "Hispanic and Latino American",
        "American Indian and Alaska Native",
        "Native Hawaiians and other Pacific Islanders",
        "Asian",
        "Missing Data"
      ),
      selected = c(
        "White",
        "Black and African American",
        "Hispanic and Latino American",
        "American Indian and Alaska Native",
        "Native Hawaiians and other Pacific Islanders",
        "Asian",
        "Missing Data"
      )
    ),

    # Veteran status filter
    shiny::checkboxGroupInput(
      inputId = ns("veteran_status"),
      label = "Is Veteran",
      choices = c(
        "Yes",
        "No",
        "Missing Data"
      ),
      selected = c(
        "Yes",
        "No",
        "Missing Data"
      )
    )
  )
}

#' filters Server Functions
#'
#' @noRd
mod_filters_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_filters_ui("filters_1")

## To be copied in the server
# mod_filters_server("filters_1")