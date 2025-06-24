#' filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filters_ui <- function(id) {
    ns <- NS(id)
    tagList(

        # Funder filter
        custom_pickerInput(
            inputId = ns("funder"),
            label = "Funder"
        ),

        # County filter
        custom_pickerInput(
            inputId = ns("county"),
            label = shiny::tagList(
                shiny::div(
                    id = ns("county_label_no_error"),
                    bslib::tooltip(
                        shiny::span(
                            "County",
                            bsicons::bs_icon("info-circle")
                        ),
                        "Showing counties associated with selected funders",
                        placement = "right"
                    )
                ),
                shiny::div(
                    id = ns("county_label_error"),
                    bslib::tooltip(
                        shiny::span(
                            "County",
                            bsicons::bs_icon("info-circle", class = "text-danger")
                        ),
                        "Please select at least one funder",
                        placement = "right"
                    )
                )
            )
        ),

        # Project filter
        custom_pickerInput(
            inputId = ns("project_filter_global"),
            label = shiny::tagList(
                shiny::div(
                    id = ns("project_label_no_error"),
                    bslib::tooltip(
                        shiny::span(
                            "Project",
                            bsicons::bs_icon("info-circle")
                        ),
                        "Showing projects associated with selected funders and counties",
                        placement = "right"
                    )
                ),
                shiny::div(
                    id = ns("project_label_error"),
                    bslib::tooltip(
                        shiny::span(
                            "Project",
                            bsicons::bs_icon("info-circle", class = "text-danger")
                        ),
                        "Please select at least one county",
                        placement = "right"
                    )
                )
            )
        ),

        # SSN De-Dup checkbox
        shiny::checkboxInput(
            inputId = ns("dedup_status_global"),
            label = "De-duplicate Participants Across Projects by SSN?",
            value = FALSE,
            width = "100%"
        ),

        # Active date filter
        shiny::dateRangeInput(
            inputId = ns("active_date_filter_global"),
            label = "Active During Period",
            start = NULL,
            end = lubridate::today(),
            min = NULL,
            max = lubridate::today()
        ),

        # Gender filter
        custom_pickerInput(
            inputId = ns("gender_filter_global"),
            label = "Gender",
            opts_selectedTextFormat = "count > 2"
        ),

        # Ethnicity filter
        custom_pickerInput(
            inputId = ns("ethnicity_filter_global"),
            label = "Ethnicity",
            opts_selectedTextFormat = "count > 2"
        ),

        # Age slider
        shiny::sliderInput(
            inputId = ns("age_filter_global"),
            label = "Age",
            min = 0,
            max = 18,
            value = c(0, 18)
        ),

        # Age missing checkbox
        shiny::checkboxInput(
            inputId = ns("age_missing_global"),
            label = "Include Participants with Missing Ages?",
            value = TRUE
        ),

        # Heads of household checkbox
        shiny::checkboxInput(
            inputId = ns("heads_of_household_global"),
            label = "Limit to Heads of Household",
            value = FALSE
        ),

        # Action button to apply filters
        bslib::input_task_button(
            id = ns("apply_filters"),
            label = "Apply"
        )
    )
}

#' filters Server Functions
#'
#' @noRd
mod_filters_server <- function(id, dm, rctv) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Update the values in the filters given the {dm} data
        ## Update funder filter
        ### Get sorted list of unique funders
        funder_choices <- dm$funder$funder |>
            unique() |>
            sort()

        ### Update filter
        shinyWidgets::updatePickerInput(
            session = session,
            inputId = "funder",
            choices = funder_choices,
            selected = funder_choices
        )

        ## Create reactive that stores projects funded by selected funders
        rctv_projects_funded_by_funders <- shiny::eventReactive(input$funder,
            {
                dm$funder |>
                    dplyr::filter(funder %in% input$funder) |>
                    dplyr::pull(project_id) |>
                    unique()
            },
            ignoreNULL = FALSE
        )

        # Improve UX based on selected funders
        shiny::observeEvent(rctv_projects_funded_by_funders(), {
            # When no funders are selected...
            if (length(rctv_projects_funded_by_funders()) == 0) {
                # Disable county input
                shinyjs::disable(id = "county")
                # Show/Hide labels
                shinyjs::show("county_label_error")
                shinyjs::hide("county_label_no_error")
            } else {
                # When at least one funder is selected...
                # Enable county input
                shinyjs::enable(id = "county")
                # Show/Hide labels
                shinyjs::show("county_label_no_error")
                shinyjs::hide("county_label_error")
            }
        })

        ## Update county filter to show only counties with projects funded by selected funders
        shiny::observeEvent(rctv_projects_funded_by_funders(), {
            ### Get counties with projects funded by selected funders
            county_choices <- dm$project_coc |>
                dplyr::filter(project_id %in% rctv_projects_funded_by_funders()) |>
                dplyr::pull(county) |>
                unique()

            ### Update filter
            shinyWidgets::updatePickerInput(
                session = session,
                inputId = "county",
                choices = county_choices,
                selected = county_choices
            )
        })

        # Improve UX based on selected county
        shiny::observeEvent(input$county,
            {
                # When no counties are selected...
                if (length(input$county) == 0) {
                    # Disable project input
                    shinyjs::disable(id = "project_filter_global")
                    # Show/Hide labels
                    shinyjs::show("project_label_error")
                    shinyjs::hide("project_label_no_error")
                } else {
                    # When at least one county is selected...
                    # Enable project input
                    shinyjs::enable(id = "project_filter_global")
                    # Show/Hide labels
                    shinyjs::show("project_label_no_error")
                    shinyjs::hide("project_label_error")
                }
            },
            ignoreNULL = FALSE
        )

        ## Update project filter (based on funder filter and county)
        ### As county choices depend on funder filter, any changes in funder
        ### selection will trigger an update of county choices. For that reason,
        ### we don't need to observe both funder and county inputs.
        shiny::observeEvent(input$county,
            {
                # Projects located in selected counties
                projects_located_in_counties <- dm$project_coc |>
                    dplyr::filter(county %in% input$county) |>
                    dplyr::pull(project_id) |>
                    unique()

                # Projects funded by selected funders and located in selected counties
                project_choices <- intersect(rctv_projects_funded_by_funders(), projects_located_in_counties)

                ### Order projects by name rather than id
                project_sorted <- dm$project |>
                    dplyr::arrange(project_name) |>
                    dplyr::left_join(y = dm$project_coc, by = "project_id") |>
                    # append coc code to project name
                    dplyr::mutate(project_name = paste0(project_name, " (", coc_code, ")")) |>
                    # Filter by Funder
                    dplyr::filter(project_id %in% project_choices)

                ### Update filter
                shinyWidgets::updatePickerInput(
                    session = session,
                    inputId = "project_filter_global",
                    choices = setNames(project_sorted$project_id, project_sorted$project_name),
                    selected = project_sorted$project_id
                )
            },
            ignoreNULL = FALSE
        )

        ## Update active date filter
        shiny::updateDateRangeInput(
            session = session,
            inputId = "active_date_filter_global",
            start = min(dm$enrollment$entry_date),
            min = min(dm$enrollment$entry_date)
        )

        ## Update gender filter
        shinyWidgets::updatePickerInput(
            session = session,
            inputId = "gender_filter_global",
            choices = levels(dm$gender$gender) |> sort(),
            selected = levels(dm$gender$gender) |> sort()
        )

        ## Update ethnicity filter
        shinyWidgets::updatePickerInput(
            session = session,
            inputId = "ethnicity_filter_global",
            choices = levels(dm$ethnicity$ethnicity) |> sort(),
            selected =  levels(dm$ethnicity$ethnicity) |> sort()
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
        clients_filtered <- shiny::eventReactive(input$apply_filters,
            {
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
                    # Remove participants who entered *after* the later active date
                    dplyr::filter(entry_date <= input$active_date_filter_global[2]) |>
                    # Remove participants who exited *before* the first active date
                    dplyr::filter(is.na(exit_date) | exit_date >= input$active_date_filter_global[1]) |>
                    # Keep one enrollment per person-organization
                    filter_most_recent_enrollment_per_group(grouping_vars = c("organization_id", "personal_id"))

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
                        # Keep one enrollment per ssn
                        filter_most_recent_enrollment_per_group(grouping_vars = "ssn")
                }

                # Return the filtered data
                out |>
                    dplyr::select(
                        enrollment_id,
                        personal_id,
                        organization_id
                    )
            },
            ignoreNULL = FALSE
        )

        waiter::waiter_hide()

        return(clients_filtered)
    })
}

## To be copied in the UI
# mod_filters_ui("filters_1")

## To be copied in the server
# mod_filters_server("filters_1")
