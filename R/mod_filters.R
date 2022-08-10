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

    shiny::fluidRow(
      shiny::column(
        width = 12,

        # Project filter
        shinyWidgets::pickerInput(
          inputId = ns("project_filter_global"),
          label = "Organization/Grantee",
          width = "460px",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          # collapse the list of selected items in the UI
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = 'count > 1'
          )
        ),

        # Submission date (quarter) filter
        shinyWidgets::pickerInput(
          inputId = ns("submission_filter_global"),
          label = "Quarter",
          width = "460px",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = 'count > 2'
          )
        ),

        # Gender filter
        shinyWidgets::pickerInput(
          inputId = ns("gender_filter_global"),
          label = "Gender",
          width = "460px",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = 'count > 2'
          )
        ),

        # Ethnicity filter
        shinyWidgets::pickerInput(
          inputId = ns("ethnicity_filter_global"),
          label = "Ethnicity",
          width = "460px",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = 'count > 2'
          )
        ),

        # Age slider
        shiny::sliderInput(
          inputId = ns("age_filter_global"),
          label = "Age",
          width = "460px",
          min = 0,
          max = 18,
          value = c(0, 18)
        ),

        # Age missing checkbox
        shiny::checkboxInput(
          inputId = ns("age_missing_global"),
          label = "Include Youth with Missing Ages?",
          value = TRUE
        ),

        # Action button to apply filters
        bs4Dash::actionButton(
          inputId = ns("apply_filters"),
          label = "Apply"
        )

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

    valid_ages <- shiny::reactive({

      dm$client |>
        dplyr::filter(!is.na(age)) |>
        dplyr::pull(age) |>
        unique()

    })

    # Update the values in the filters given the {dm} data
    shiny::observe({

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "project_filter_global",
        choices = unique( dm$project$project_name ) |> sort(),
        selected = unique( dm$project$project_name ) |> sort()
      )

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "submission_filter_global",
        choices = unique( dm$submission$quarter ) |> sort(),
        selected = unique( dm$submission$quarter ) |> sort()
      )

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "gender_filter_global",
        choices = unique( dm$gender$gender ) |> sort(),
        selected = unique( dm$gender$gender ) |> sort()
      )

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "ethnicity_filter_global",
        choices = unique( dm$ethnicity$ethnicity ) |> sort(),
        selected = unique( dm$ethnicity$ethnicity ) |> sort()
      )

      shiny::updateSliderInput(
        session = session,
        inputId = "age_filter_global",
        min = min( valid_ages() ),
        max = max( valid_ages() ),
        value = c(
          min( valid_ages() ),
          max( valid_ages() )
        )
      )

    })

    # Created filtered {dm} data
    dm_filtered <- shiny::eventReactive(input$apply_filters, {

      dm_out <- dm |>
        dm::dm_filter(project, project_name %in% input$project_filter_global) |>
        dm::dm_filter(submission, quarter %in% input$submission_filter_global) |>
        dm::dm_filter(gender, gender %in% input$gender_filter_global) |>
        dm::dm_filter(ethnicity, ethnicity %in% input$ethnicity_filter_global)

      if (input$age_missing_global) {

        dm_out <- dm_out |>
          dm::dm_filter(
            client,
            (age >= input$age_filter_global[1] & age <= input$age_filter_global[2]) | is.na(age)
          )

      } else {

        dm_out <- dm_out |>
          dm::dm_filter(
            client,
            age >= input$age_filter_global[1] & age <= input$age_filter_global[2]
          )

      }

      # Close the global filters pane when the "Apply" button is clicked
      # This doesn't work; since "control_bar" div is not in this module
      # bs4Dash::updateControlbar(
      #   session = session,
      #   id = "control_bar",
      #   collapsed = TRUE
      # )

      dm_out |>
        dm::dm_apply_filters()

    }, ignoreNULL = FALSE)

    return(dm_filtered)

  })
}

## To be copied in the UI
# mod_filters_ui("filters_1")

## To be copied in the server
# mod_filters_server("filters_1")
