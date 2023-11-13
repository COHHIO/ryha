#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_ui <- function(id){
  ns <- NS(id)
  tagList(

    bs4Dash::box(
      title = shiny::h1(
        shiny::span("Welcome to the"),
        shiny::span(shiny::em("Youth Homelessness Data Dashboard"))
      ),
      width = 12,
      status = "primary",
      collapsible = FALSE,

      # Add vertical-centered logos
      shiny::fluidRow(

        shiny::column(
          width = 3,
          shiny::img(src = "www/odh_logo.png", width = "100%")
        ) |>
          shiny::tagAppendAttributes(class = "vertical-center"),

        shiny::column(
          width = 6,
          shiny::img(src = "www/cohhio_logo.png", width = "100%")
        ) |>
          shiny::tagAppendAttributes(class = "vertical-center"),

        shiny::column(
          width = 3,
          shiny::img(src = "www/ketchbrook_logo.png", width = "100%")
        ) |>
          shiny::tagAppendAttributes(class = "vertical-center")

      )

    )

  )

}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
