#' help_btn UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_help_btn_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::actionButton(
      inputId = ns("jump_to_help"),
      class = "btn btn-info btn-lg",
      label = "Help",
      width = "200px"
    )

  )
}

#' help_btn Server Functions
#'
#' @noRd
mod_help_btn_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$jump_to_help, {

      bs4Dash::updatebs4TabItems(
        session = session,
        inputId = "left_sidebar_menu",
        selected = 4# nrow(define_menu())   # "Help" should always be the last menu item
      )

    })

  })
}

## To be copied in the UI
# mod_help_btn_ui("help_btn_1")

## To be copied in the server
# mod_help_btn_server("help_btn_1")
