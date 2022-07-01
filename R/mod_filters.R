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
    # To create the filters, I decided to use checkboxGroupInput.
    #  The reason for that is that all options get displayed when you open
    #  the controlBar, and you can quickly select or unselect any category.
    #  When using a selectInput that accepted multiple options, in order to
    #  remove one you had to click on the option and delete it with the keyboard.
    #  For now, I find the checkboxGroupInput more appealing. This can change in
    #  the future.

    # Gender filter
    # Values are hardcoded, they come from data wrangling step in fct_create_dm.R
    shiny::checkboxGroupInput(
      inputId = ns("gender"),
      label = shiny::h3("Gender"),
      choices = c(
        "Female",
        "Male",
        "No Single Gender",
        "Transgender",
        "Questioning",
        "Missing Data"
      ),
      selected = c(
        "Female",
        "Male",
        "No Single Gender",
        "Transgender",
        "Questioning",
        "Missing Data"
      )
    ),

    # Ethnicity filter
    # Values are hardcoded, they come from data wrangling step in fct_create_dm.R
    shiny::checkboxGroupInput(
      inputId = ns("ethnicity"),
      label = shiny::h3("Ethnicity"),
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
    # Values are hardcoded, they come from data wrangling step in fct_create_dm.R
    shiny::checkboxGroupInput(
      inputId = ns("veteran_status"),
      label = shiny::h3("Is Veteran"),
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
mod_filters_server <- function(id, dm){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Here we create and return a filtered dm object.
    # dm is not reactive because it is computed when we launch the app.
    # This approach might not be the best, but it works.
    # Maybe in the future we can refactor code.
    my_dm_filtered <- shiny::reactive({
      dm |>
        dm::dm_filter(table_client,
                      gender %in% input$gender,
                      ethnicity %in% input$ethnicity,
                      veteran_status %in% input$veteran_status) |>
        dm::dm_apply_filters()
    })

    return(my_dm_filtered)

  })
}

## To be copied in the UI
# mod_filters_ui("filters_1")

## To be copied in the server
# mod_filters_server("filters_1")
