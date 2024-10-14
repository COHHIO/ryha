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

        # Funder filter
        shinyWidgets::pickerInput(
          inputId = ns("funder"),
          label = "Funder",
          width = "460px",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          # collapse the list of selected items in the UI
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = 'count > 1',
            container = "body"
          )
        ),

        # Project filter
        shinyWidgets::pickerInput(
          inputId = ns("project_filter_global"),
          label = with_popover(text = "Project", title = NULL, content = "Showing project(s) funded by selected funder(s)"),
          width = "460px",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          # collapse the list of selected items in the UI
          options = list(
            `actions-box` = TRUE,
            `selected-text-format` = 'count > 1',
            container = "body"
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

        # Heads of household checkbox
        shiny::checkboxInput(
          inputId = ns("heads_of_household_global"),
          label = "Limit to Heads of Household",
          value = FALSE
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
mod_filters_server <- function(id, dm, rctv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Update the values in the filters given the {dm} data

    ## Update funder filter

    ### Get sorted list of unique funders
    funder_choices <- dm$funder$funder |> unique() |> sort()

    ### Update filter
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "funder",
      choices = funder_choices,
      selected = funder_choices
    )

    ## Update project filter (based on funder filter)
    shiny::observeEvent(input$funder, {

      # Projects funded by selected funder(s)
      projects_funded_by_funders <- dm$funder |>
        dplyr::filter(funder %in% input$funder) |>
        dplyr::pull(project_id) |>
        unique()
      
      ### Order projects by name rather than id
      project_sorted <- dm$project |>
        dplyr::arrange(project_name) |>
        dplyr::left_join(y = dm$project_coc, by = "project_id") |>
        # append coc code to project name
        dplyr::mutate(project_name = paste0(project_name, " (", coc_code, ")")) |>
        # Filter by Funder
        dplyr::filter(project_id %in% projects_funded_by_funders)

      ### Update filter
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "project_filter_global",
        choices = setNames(project_sorted$project_id, project_sorted$project_name),
        selected = project_sorted$project_id,
        choicesOpt = list(
          style = rep_len(
            "font-size: 75%;",
            project_sorted$project_name |> length()
          )
        )
      )

    })

    ## Update active date filter
    shiny::updateDateRangeInput(
      session = session,
      inputId = "active_date_filter_global",
      start = min( dm$enrollment$entry_date ),
      min = min( dm$enrollment$entry_date )
    )

    ## Update gender filter
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "gender_filter_global",
      choices = unique( dm$gender$gender ) |> sort(),
      selected = unique( dm$gender$gender ) |> sort()
    )

    ## Update ethnicity filter
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "ethnicity_filter_global",
      choices = unique( dm$ethnicity$ethnicity ) |> sort(),
      selected = unique( dm$ethnicity$ethnicity ) |> sort()
    )

    ## Update age filter

    ### Grab the ages found in the `client` table
    valid_ages <- dm$client |>
      dplyr::filter(!is.na(age)) |>
      dplyr::pull(age) |>
      unique()

    ### Update filter
    shiny::updateSliderInput(
      session = session,
      inputId = "age_filter_global",
      min = min(valid_ages),
      max = max(valid_ages),
      value = c(
        min(valid_ages),
        max(valid_ages)
      )
    )

    # Disable the "dedup_status_global" check-box if only 1 program is selected
    shiny::observeEvent(input$project_filter_global, {

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

      # Filter dm$client by age
      client <- dm$client |>
        dplyr::filter(
          dplyr::between(
            x = age,
            left = input$age_filter_global[1],
            right = input$age_filter_global[2]
          ) | is.na(age)
        )

      ## Remove youth with missing ages accordingly
      if (input$age_missing_global == FALSE) {
        client <- client |>
          dplyr::filter(!is.na(age))
      }

      # Filter dm$gender by gender
      gender <- dm$gender |>
        dplyr::filter(gender %in% input$gender_filter_global)

      # Filter dm$ethnicity by ethnicity
      ethnicity <- dm$ethnicity |>
        dplyr::filter(ethnicity %in% input$ethnicity_filter_global)

      # Filter dm$enrollment
      enrollment <- dm$enrollment |>
        # Add exit date
        dplyr::left_join(
          dplyr::select(dm$exit, enrollment_id, personal_id, organization_id, exit_date),
          by = c("organization_id", "personal_id", "enrollment_id")
        ) |>
        # Filter by project
        dplyr::filter(project_id %in% input$project_filter_global) |>
        # Remove individuals who entered *after* the later active date
        dplyr::filter(entry_date <= input$active_date_filter_global[2]) |>
        # Remove individuals who exited *before* the first active date
        dplyr::filter(is.na(exit_date) | exit_date >= input$active_date_filter_global[1]) |>
        # Group data to select one enrollment per person-organization
        dplyr::group_by(organization_id, personal_id) |>
        # Apply filters one at the time until we are left with one enrollment per person-organization
        ## Keep enrollment(s) without exit date (or with the most recent exit date if all enrollments have an exit date)
        dplyr::mutate(aux_exit = dplyr::case_when(
          is.na(exit_date) ~ as.Date("9999-01-01"),
          TRUE ~ exit_date
        )) |>
        dplyr::filter(aux_exit == max(aux_exit)) |>
        dplyr::select(-aux_exit) |>
        ## Keep enrollment(s) that have the latest entry date
        dplyr::filter(entry_date == max(entry_date)) |>
        ## Keep enrollment(s) that have the latest date updated
        dplyr::filter(date_updated == max(date_updated)) |>
        ## Keep enrollment with the highest enrollment_id
        dplyr::filter(enrollment_id == max(enrollment_id)) |>
        # Ungroup data
        dplyr::ungroup()
        # At this point we should have one enrollment per person-organization

      # Filter head of household accordingly
      if (input$heads_of_household_global == TRUE) {
        enrollment <- enrollment |>
          dplyr::filter(relationship_to_ho_h == "Self (head of household)")
      }

      out <- enrollment |>
        dplyr::semi_join(client, by = c("organization_id", "personal_id")) |>
        dplyr::semi_join(gender, by = c("organization_id", "personal_id")) |>
        dplyr::semi_join(ethnicity, by = c("organization_id", "personal_id"))

      # De-duplicate youth across projects by ssn accordingly
      if (input$dedup_status_global == TRUE) {
        out <- out |>
          dplyr::left_join(
            dplyr::select(client, organization_id, personal_id, ssn, ssn_data_quality),
            by = c("organization_id", "personal_id")
          ) |>
          # Keep youth with "Full SSN reported"
          dplyr::filter(ssn_data_quality == "Full SSN reported") |>
          # Remove youth with "Full SSN reported" that have missing SSN
          dplyr::filter(!is.na(ssn)) |>
          # Group data to select one enrollment per ssn
          dplyr::group_by(ssn) |>
          # Apply filters one at the time until we are left with one enrollment per ssn
          ## Keep enrollment(s) without exit date (or with the most recent exit date if all enrollments have an exit date)
          dplyr::mutate(aux_exit = dplyr::case_when(
            is.na(exit_date) ~ as.Date("9999-01-01"),
            TRUE ~ exit_date
          )) |>
          dplyr::filter(aux_exit == max(aux_exit)) |>
          dplyr::select(-aux_exit) |>
          ## Keep enrollment(s) that have the latest entry date
          dplyr::filter(entry_date == max(entry_date)) |>
          ## Keep enrollment(s) that have the latest date updated
          dplyr::filter(date_updated == max(date_updated)) |>
          ## Keep enrollment with the highest enrollment_id
          dplyr::filter(enrollment_id == max(enrollment_id)) |>
          dplyr::ungroup()
      }

      # Update the reactiveValues list of selected projects
      rctv$selected_projects <- input$project_filter_global

      # Return the filtered data
      out |>
        dplyr::select(
          enrollment_id,
          personal_id,
          organization_id
        )

    }, ignoreNULL = FALSE)

    waiter::waiter_hide()

    return(clients_filtered)

  })
}

## To be copied in the UI
# mod_filters_ui("filters_1")

## To be copied in the server
# mod_filters_server("filters_1")
