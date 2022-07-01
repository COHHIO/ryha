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
    # Organization filter
    shinyWidgets::pickerInput(
      inputId = ns("organization"),
      label = shiny::h3("Organization", class = "control-filter-label"),
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),

    # Gender filter
    # Values are hardcoded, they come from data wrangling step in fct_create_dm.R
    shinyWidgets::pickerInput(
      inputId = ns("gender"),
      label = shiny::h3("Gender", class = "control-filter-label"),
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
      ),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),

    # Ethnicity filter
    # Values are hardcoded, they come from data wrangling step in fct_create_dm.R
    shinyWidgets::pickerInput(
      inputId = ns("ethnicity"),
      label = shiny::h3("Ethnicity", class = "control-filter-label"),
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
      ),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),

    # Veteran status filter
    # Values are hardcoded, they come from data wrangling step in fct_create_dm.R
    shinyWidgets::pickerInput(
      inputId = ns("veteran_status"),
      label = shiny::h3("Is Veteran", class = "control-filter-label"),
      choices = c(
        "Yes",
        "No",
        "Missing Data"
      ),
      selected = c(
        "Yes",
        "No",
        "Missing Data"
      ),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),

    # Add button to trigger filters
    bs4Dash::actionButton(
      inputId = ns("apply_filters"),
      label = "Apply Filters"
    )
  )
}

#' filters Server Functions
#'
#' @noRd
mod_filters_server <- function(id, dm){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # In order to get the complete list of organizations, we use the projects
    #  table from the dm object that gets created in the server
    list_organizations <- dm$table_project |>
      dplyr::pull(project_name) |>
      unique() |>
      sort()

    # Update the selectInput when list_organizations gets computed
    # This observeEvent will trigger once because list_quarters is not reactive.
    observeEvent(list_organizations, {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "organization",
        choices = list_organizations,
        selected = list_organizations
      )
    })

    # Here we create and return a filtered dm object.
    # dm is not reactive because it is computed when we launch the app.
    # This approach might not be the best, but it works.
    # Maybe in the future we can refactor code.
    my_dm_filtered <- shiny::eventReactive(input$apply_filters, {
      dm |>
        dm::dm_filter(table_client,
                      gender %in% input$gender,
                      ethnicity %in% input$ethnicity,
                      veteran_status %in% input$veteran_status) |>
        dm::dm_filter(table_project,
                      project_name %in% input$organization) |>
        dm::dm_apply_filters()
    }, ignoreNULL = FALSE)

    return(my_dm_filtered)

  })
}

## To be copied in the UI
# mod_filters_ui("filters_1")

## To be copied in the server
# mod_filters_server("filters_1")
