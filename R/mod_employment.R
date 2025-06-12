#' employment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_employment_ui <- function(id) {
    ns <- NS(id)
    tagList(
        bslib::layout_columns(
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_with_records"),
                title = "# of Head of Household and/or Adults with Records",
                tooltip = "Responses within those records may still be missing"
            ),
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_without_records"),
                title = "# of Head of Household and/or Adults without Records"
            ),
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_employed"),
                title = "# of Head of Household and/or Adults Employed"
            )
        ),
        custom_card(
            bslib::card_header(
                with_popover(
                    text = "# of Head of Household and/or Adults by Employment Status",
                    content = link_section("R6 Employment Status")
                )
            ),
            echarts4r::echarts4rOutput(outputId = ns("employed_chart"), height = "100%")
        ),
        bslib::layout_columns(
            custom_card(
                bslib::card_header(
                    with_popover(
                        text = "# of Employed Participants by Employment Type",
                        content = link_section("R6 Employment Status")
                    )
                ),
                echarts4r::echarts4rOutput(outputId = ns("employment_type_chart"), height = "100%")
            ),
            custom_card(
                bslib::card_header(
                    with_popover(
                        text = "# of Not Employed Participants by Reason Not Employed",
                        content = link_section("R6 Employment Status")
                    )
                ),
                echarts4r::echarts4rOutput(outputId = ns("not_employed_reason_chart"), height = "100%")
            )
        ),
        custom_card(
            bslib::card_header(
                with_popover(
                    text = "Changes in Employed Status (Entry --> Exit)",
                    content = link_section("R6 Employment Status")
                )
            ),
            echarts4r::echarts4rOutput(outputId = ns("employed_sankey_chart"), height = "100%")
        )
    )
}

#' employment Server Functions
#'
#' @noRd
mod_employment_server <- function(id, employment_data, clients_filtered, heads_of_household_and_adults) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Filter Data ####
        employment_data_filtered <- shiny::reactive({
            filter_data(employment_data, clients_filtered()) |>
                dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
        })

        most_recent_data_per_enrollment <- shiny::reactive({
            employment_data_filtered() |>
                # Employment data should be collected only at Project start and Project exit
                dplyr::filter(data_collection_stage %in% c("Project start", "Project exit")) |>
                filter_most_recent_data_per_enrollment()
        })

        # Value Boxes ####
        mod_value_box_server(
            id = "n_heads_of_household_and_adults_with_records",
            rctv_data = most_recent_data_per_enrollment
        )

        mod_value_box_server(
            id = "n_heads_of_household_and_adults_without_records",
            rctv_data = shiny::reactive({
                filter_data(heads_of_household_and_adults, clients_filtered()) |>
                    dplyr::anti_join(
                        most_recent_data_per_enrollment(),
                        by = c("enrollment_id", "personal_id", "organization_id")
                    )
            })
        )

        mod_value_box_server(
            id = "n_heads_of_household_and_adults_employed",
            rctv_data = shiny::reactive({
                most_recent_data_per_enrollment() |>
                    dplyr::filter(employed == "Yes")
            })
        )

        # Charts ####
        ## Employed ####
        output$employed_chart <- echarts4r::renderEcharts4r({
            most_recent_data_per_enrollment() |>
                dplyr::count(employed, .drop = FALSE) |>
                bar_chart(
                    x = "employed",
                    y = "n"
                )
        })

        ## Employment Type ####
        output$employment_type_chart <- echarts4r::renderEcharts4r({
            most_recent_data_per_enrollment() |>
                dplyr::filter(employed == "Yes") |>
                dplyr::count(employment_type, .drop = FALSE) |>
                bar_chart(
                    x = "employment_type",
                    y = "n"
                )
        })

        ## Not Employed Reason ####
        output$not_employed_reason_chart <- echarts4r::renderEcharts4r({
            most_recent_data_per_enrollment() |>
                dplyr::filter(employed == "No") |>
                dplyr::count(not_employed_reason, .drop = FALSE) |>
                bar_chart(
                    x = "not_employed_reason",
                    y = "n"
                )
        })

        ## Sankey
        output$employed_sankey_chart <- echarts4r::renderEcharts4r({
            employment_data_filtered() |>
                prepare_sankey_data(
                    response_col = "employed",
                    response_vals = c("Yes", "No")
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
# mod_employment_ui("employment_1")

## To be copied in the server
# mod_employment_server("employment_1")
