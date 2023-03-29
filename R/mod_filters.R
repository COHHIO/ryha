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
          label = "Project",
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
          inputId = ns("dedup_status_global"),
          label = "De-duplicate Youth Across Projects by SSN?",
          value = FALSE,
          width = "100%"
        ),


        # Active date filter
        shiny::dateRangeInput(
          inputId = ns("active_date_filter_global"),
          label = "Active During Period",
          width = "460px",
          start = NULL,
          end = lubridate::today(),
          min = NULL,
          max = lubridate::today()
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
        ),

        # Add padding under "Apply" button
        shiny::br(),
        shiny::br(),
        shiny::br()

      )
    )

  )
}

#' filters Server Functions
#'
#' @noRd
mod_filters_server <- function(id, dm, w, rctv){
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

      shiny::req(dm)

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "project_filter_global",
        choices = unique( dm$project$project_name ) |> sort(),
        selected = unique( dm$project$project_name ) |> sort(),
        choicesOpt = list(
          style = rep_len(
            "font-size: 75%;",
            unique( dm$project$project_name ) |> length()
          )
        )
      )

      shiny::updateDateRangeInput(
        session = session,
        inputId = "active_date_filter_global",
        start = min( dm$enrollment$entry_date ),
        min = min( dm$enrollment$entry_date )
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

      w$hide()

    })

    # Disable the "dedup_status_global" check-box if only 1 program is selected
    shiny::observe({

      if (length(input$project_filter_global) < 2L) {

        shiny::updateCheckboxInput(
          session = session,
          inputId = "dedup_status_global",
          value = FALSE
        )

        shinyjs::disable(id = "dedup_status_global")

      } else {

        shinyjs::enable(id = "dedup_status_global")

      }

    })

    # Create filtered {dm} data
    clients_filtered <- shiny::eventReactive(input$apply_filters, {

      # Filter client data to allow/disallow missing ages
      if (input$age_missing_global == TRUE) {

        client <- dm$client |>
          dplyr::filter(
            dplyr::between(
              x = age,
              left = input$age_filter_global[1],
              right = input$age_filter_global[2]
            ) | is.na(age)
          )

      } else {

        client <- dm$client |>
          dplyr::filter(
            dplyr::between(
              x = age,
              left = input$age_filter_global[1],
              right = input$age_filter_global[2]
            )
          )

      }

      out <- dm$project |>
        dplyr::filter(project_name %in% input$project_filter_global) |>
        dplyr::select(project_id) |>
        dplyr::inner_join(
          dm$enrollment |>
            # Remove individuals who entered *after* the later active date
            dplyr::filter(
              entry_date <= input$active_date_filter_global[2]
            ),
          by = "project_id"
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        dplyr::inner_join(
          client,
          by = c("personal_id", "organization_id")
        ) |>
        dplyr::inner_join(
          dm$gender |>
            dplyr::filter(
              gender %in% input$gender_filter_global
            ) |>
            dplyr::select(personal_id, organization_id),
          by = c("personal_id", "organization_id")
        ) |>
        dplyr::inner_join(
          dm$ethnicity |>
            dplyr::filter(
              ethnicity %in% input$ethnicity_filter_global
            ) |>
            dplyr::select(personal_id, organization_id),
          by = c("personal_id", "organization_id")
        ) |>
        dplyr::left_join(
          dm$exit |>
            dplyr::select(personal_id, organization_id, exit_date),
          by = c("personal_id", "organization_id")
        ) |>
        # Remove individuals who exited *before* the first active date
        dplyr::filter(is.na(exit_date) | exit_date >= input$active_date_filter_global[1])

      # If the "De-duplicate by SSN" checkbox is clicked, limit the data to the
      # most recent `personal_id` value for each unique SSN
      if (input$dedup_status_global == TRUE) {

        out <- out |>
          dplyr::filter(ssn_data_quality == "Full SSN reported") |>
          dplyr::arrange(ssn, dplyr::desc(date_updated)) |>
          dplyr::distinct(ssn, .keep_all = TRUE)

      }

      # Update the reactiveValues list of selected projects
      rctv$selected_projects <- input$project_filter_global

      # Return the filtered data
      out |>
        dplyr::distinct(personal_id, organization_id)

    }, ignoreNULL = FALSE)

    return(clients_filtered)

  })
}

## To be copied in the UI
# mod_filters_ui("filters_1")

## To be copied in the server
# mod_filters_server("filters_1")
