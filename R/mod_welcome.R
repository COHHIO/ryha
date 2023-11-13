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
      title = shiny::HTML("<h1>Welcome to the <em>Youth Homelessness Data Dashboard</em></h1>"),
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

    ),

    shiny::fluidRow(

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = "Partnership",
          width = 12,
          status = "info",
          style = "text-align: justify",

          shiny::HTML(
            "This app is the result of a partnership between:
            <ul>
              <li> <a href='https://odh.ohio.gov/'>Ohio Department of Health (ODH)</a>,
              <li> <a href='https://cohhio.org/'>Coalition on Homelessness and Housing in Ohio (COHHIO)</a>,
              <li> <a href='https://www.ketchbrookanalytics.com'>Ketchbrook Analytics</a>
            </ul>
            "
          )
        ),

        bs4Dash::box(
          title = "Contact & Help",
          width = 12,
          status = "info",
          style = "text-align: justify",

          shiny::HTML(
            "This app utilizes the HMIS CSV to provide information on the ODH
            program. If you have any questions about the app, please email
            Amanda Wilson, Youth Housing Initiative Director at COHHIO, at
            <a href='mailto:amandawilson@cohhio.org'>amandawilson@cohhio.org</a>

            <br><br>

            Please see the <a href='#tab-help_page'>Help</a> page on the left-hand
            sidebar menu for more information on how to navigate the app.
            "
          )
        )
      ),

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = "Program Development",
          width = 12,
          status = "info",
          style = "text-align: justify",

          shiny::HTML(
            "As part of ODH's youth homelessness grant program, <strong>COHHIO</strong>
            was contracted to assist the department in program development using
            data-driven approaches.

            <br><br>

            <strong>COHHIO</strong> supported the department in establishing
            data collection requirements and providing technical assistance
            to sub-recipients to standardize data collection across the program.

            <br><br>

            This was best accomplished using the <strong>Housing and Urban
            Development</strong> (HUD)-required <strong>Homeless Management
            Information System</strong> (HMIS).

            <br><br>


            In 2022, <strong>COHHIO</strong> entered into a contract with
            <a href='https://www.ketchbrookanalytics.com'>Ketchbrook Analytics</a>
            to assist by analyzing large volumes of data from sub-grantees
            and quantifying trends across data sets.
            "
          )
        )

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
