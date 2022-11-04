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

        # SSN De-Dup checkbox
        shiny::checkboxInput(
          inputId = ns("dedup_status"),
          label = "De-duplicate Youth Across Programs by SSN?",
          value = FALSE,
          width = "100%"
        ),

        # Entry date filter
        shiny::dateInput(
          inputId = ns("min_entry_date_filter_global"),
          label = "Entry Date (on or after)",
          width = "460px",
          value = NULL,
          min = NULL,
          max = NULL
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

    # Grab the min & max ages found in the `client` table
    valid_ages <- shiny::reactive({

      dm$client |>
        dplyr::filter(!is.na(age)) |>
        dplyr::pull(age) |>
        unique()

    })

    # Update the values in the filters given the {dm} data
    shiny::observe({

      shiny::showNotification(
        "Please Wait...",
        id = "load_notification",
        duration = NULL,
        type = "warning",
        closeButton = FALSE
      )

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "project_filter_global",
        choices = unique( dm$project$project_name ) |> sort(),
        selected = unique( dm$project$project_name ) |> sort()
      )

      shiny::updateDateInput(
        session = session,
        inputId = "min_entry_date_filter_global",
        value = min( dm$enrollment$entry_date ),
        min = min( dm$enrollment$entry_date ),
        max = max( dm$enrollment$entry_date )
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

      shiny::removeNotification(
        id = "load_notification"
      )

    })

    # Disable the "dedup_status" check-box if only 1 program is selected
    shiny::observe({

      if (length(input$project_filter_global) < 2L) {

        shiny::updateCheckboxInput(
          session = session,
          inputId = "dedup_status",
          value = FALSE
        )

        shinyjs::disable(id = "dedup_status")

      } else {

        shinyjs::enable(id = "dedup_status")

      }

    })

    # Created filtered {dm} data
    clients_filtered <- shiny::eventReactive(input$apply_filters, {

     dm$project |>
        dplyr::filter(project_name %in% input$project_filter_global) |>
        dplyr::select(project_id) |>
        dplyr::inner_join(
          dm$enrollment,
          by = "project_id"
        ) |>
        dplyr::distinct(personal_id, software_name) |>
        dplyr::inner_join(
          dm$client |>
            dplyr::filter(
              dplyr::between(
                x = age,
                left = input$age_filter_global[1],
                right = input$age_filter_global[2]
              )
            ),
          by = c("personal_id", "software_name")
        ) |>
        dplyr::inner_join(
          dm$gender |>
            dplyr::filter(
              gender %in% input$gender_filter_global
            ),
          by = c("personal_id", "software_name")
        ) |>
        dplyr::inner_join(
          dm$ethnicity |>
            dplyr::filter(
              ethnicity %in% input$ethnicity_filter_global
            ),
          by = c("personal_id", "software_name")
        ) |>
        dplyr::distinct(personal_id, software_name, ssn, ssn_data_quality)

      # if (input$age_missing_global) {
      #
      #   dm_out <- dm_out |>
      #     dm::dm_filter(
      #       client,
      #       (age >= input$age_filter_global[1] & age <= input$age_filter_global[2]) | is.na(age)
      #     )
      #
      # } else {
      #
      #   dm_out <- dm_out |>
      #     dm::dm_filter(
      #       client,
      #       age >= input$age_filter_global[1] & age <= input$age_filter_global[2]
      #     )
      #
      # }

      # Close the global filters pane when the "Apply" button is clicked
      # This doesn't work; since "control_bar" div is not in this module
      # bs4Dash::updateControlbar(
      #   session = session,
      #   id = "control_bar",
      #   collapsed = TRUE
      # )

    }, ignoreNULL = FALSE)

    return(clients_filtered)

  })
}

## To be copied in the UI
# mod_filters_ui("filters_1")

## To be copied in the server
# mod_filters_server("filters_1")
