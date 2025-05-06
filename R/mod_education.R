#' education UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_education_ui <- function(id) {
    ns <- NS(id)
    tagList(
        bslib::layout_columns(
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_with_education_data"),
                title = "# of Head of Household and/or Adults with Education Data",
                tooltip = "Head of Household and/or Adults included in Overview who also appear in Education records"
            ),
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_missing"),
                title = "# of Head of Household and/or Adults Missing",
                tooltip = "Head of Household and/or Adults included in Overview without a matching Education record"
            )
        ),
        bslib::layout_columns(
            bslib::card(
                bslib::card_header(shiny::h2("Last Grade Completed")),
                custom_card(
                    bslib::card_header(
                        with_popover(
                            text = "# of Head of Household and/or Adults by Last Grade Completed Group",
                            content = shiny::tagList(
                                shiny::span("Response categories have been grouped to improve chart readability."),
                                shiny::br(),
                                link_section("R4 Last Grade Completed")
                            )
                        )
                    ),
                    echarts4r::echarts4rOutput(outputId = ns("last_grade_completed_chart"), height = "100%")
                ),
                custom_card(
                    bslib::card_header(
                        with_popover(
                            text = "Changes in Last Grade Completed Group (Entry --> Exit)",
                            content = shiny::tagList(
                                shiny::span("Response categories have been grouped to improve chart readability."),
                                shiny::br(),
                                link_section("R4 Last Grade Completed")
                            )
                        )
                    ),
                    echarts4r::echarts4rOutput(outputId = ns("last_grade_completed_sankey_chart"), height = "100%")
                )
            ),
            bslib::card(
                bslib::card_header(shiny::h2("School Status")),
                custom_card(
                    bslib::card_header(
                        with_popover(
                            text = "# of Head of Household and/or Adults by School Status",
                            content = link_section("R5 School Status")
                        )
                    ),
                    echarts4r::echarts4rOutput(outputId = ns("school_status_chart"), height = "100%")
                ),
                custom_card(
                    bslib::card_header(
                        with_popover(
                            text = "Changes in School Status (Entry --> Exit)",
                            content = link_section("R5 School Status")
                        )
                    ),
                    echarts4r::echarts4rOutput(outputId = ns("school_status_sankey_chart"), height = "100%")
                )
            )
        )
    )
}

#' education Server Functions
#'
#' @noRd
mod_education_server <- function(id, education_data, clients_filtered, heads_of_household_and_adults) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Filter Data ####
        education_data_filtered <- shiny::reactive({
            filter_data(education_data, clients_filtered()) |>
                dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
        })

        most_recent_data_per_enrollment <- shiny::reactive({
            education_data_filtered() |>
                filter_most_recent_data_per_enrollment()
        })

        # Value Boxes ####
        mod_value_box_server(
            id = "n_heads_of_household_and_adults_with_education_data",
            rctv_data = most_recent_data_per_enrollment
        )

        mod_value_box_server(
            id = "n_heads_of_household_and_adults_missing",
            rctv_data = shiny::reactive({
                filter_data(heads_of_household_and_adults, clients_filtered()) |>
                    dplyr::anti_join(
                        most_recent_data_per_enrollment(),
                        by = c("enrollment_id", "personal_id", "organization_id")
                    )
            })
        )

        # Charts ####
        ## Last Grade Completed ####
        output$last_grade_completed_chart <- echarts4r::renderEcharts4r({
            most_recent_data_per_enrollment() |>
                dplyr::count(last_grade_completed_grouped, .drop = FALSE) |>
                bar_chart(
                    x = "last_grade_completed_grouped",
                    y = "n"
                )
        })

        ## Sankey ####
        output$last_grade_completed_sankey_chart <- echarts4r::renderEcharts4r({
            education_data_filtered() |>
                prepare_sankey_data(
                    response_col = "last_grade_completed_grouped",
                    response_vals = c(
                        "Less than Grade 5",
                        "Grades 5-8",
                        "Grades 9-11",
                        "Some College",
                        "High school diploma/GED",
                        "College Degree/Vocational"
                    )
                ) |>
                sankey_chart(
                    entry_status = "Entry",
                    exit_status = "Exit",
                    count = "n"
                )
        })

        ## School Status ####
        output$school_status_chart <- echarts4r::renderEcharts4r({
            most_recent_data_per_enrollment() |>
                dplyr::count(school_status, .drop = FALSE) |>
                bar_chart(
                    x = "school_status",
                    y = "n"
                )
        })

        ## Sankey ####
        output$school_status_sankey_chart <- echarts4r::renderEcharts4r({
            education_data_filtered() |>
                prepare_sankey_data(
                    response_col = "school_status",
                    response_vals = c(
                        "Obtained GED",
                        "Graduated from high school",
                        "Attending school regularly",
                        "Attending school irregularly",
                        "Suspended",
                        "Expelled"
                    )
                ) |>
                sankey_chart(
                    entry_status = "Entry",
                    exit_status = "Exit",
                    count = "n"
                )
        })
    })
}

## To be copied in the UI
# mod_education_ui("education_1")

## To be copied in the server
# mod_education_server("education_1")
