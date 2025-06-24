#' domestic_violence UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_domestic_violence_ui <- function(id) {
    ns <- NS(id)
    tagList(
        bslib::layout_columns(
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_with_records"),
                title = "Head of Household and/or Adults with Records",
                tooltip = "Responses within those records may still be missing"
            ),
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_without_records"),
                title = "Head of Household and/or Adults without Records"
            ),
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_who_are_domestic_violence_victim"),
                title = "Head of Household and/or Adults that Experienced Domestic Violence"
            )
        ),
        custom_card(
            bslib::card_header(
                with_popover(
                    text = "Head of Household and/or Adults by Domestic Violence Victim Response",
                    content = link_section("4.11 Domestic Violence")
                )
            ),
            echarts4r::echarts4rOutput(outputId = ns("victim_chart"), height = "100%")
        ),
        bslib::layout_columns(
            custom_card(
                bslib::card_header(
                    with_popover(
                        text = "Domestic Violence Victims by When Occurred Response",
                        content = link_section("4.11 Domestic Violence")
                    )
                ),
                echarts4r::echarts4rOutput(outputId = ns("when_occurred_chart"), height = "100%")
            ),
            custom_card(
                bslib::card_header(
                    with_popover(
                        text = "Domestic Violence Victims by Currently Fleeing Response",
                        content = link_section("4.11 Domestic Violence")
                    )
                ),
                echarts4r::echarts4rOutput(outputId = ns("currently_fleeing_chart"), height = "100%")
            ),
        )
    )
}

#' domestic_violence Server Functions
#'
#' @noRd
mod_domestic_violence_server <- function(id, domestic_violence_data, clients_filtered, heads_of_household_and_adults) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Filter Data ####
        domestic_violence_data_filtered <- shiny::reactive({
            filter_data(domestic_violence_data, clients_filtered()) |>
                dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
        })

        most_recent_data_per_enrollment <- shiny::reactive({
            domestic_violence_data_filtered() |>
                # Domestic violence data is not expected to be collected at Project exit
                dplyr::filter(data_collection_stage != "Project exit") |>
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
            id = "n_heads_of_household_and_adults_who_are_domestic_violence_victim",
            rctv_data = shiny::reactive({
                most_recent_data_per_enrollment() |>
                    dplyr::filter(domestic_violence_survivor == "Yes")
            })
        )

        # Charts ####
        output$victim_chart <- echarts4r::renderEcharts4r({
            most_recent_data_per_enrollment() |>
                dplyr::count(domestic_violence_survivor, .drop = FALSE) |>
                bar_chart(
                    x = "domestic_violence_survivor",
                    y = "n"
                )
        })

        output$when_occurred_chart <- echarts4r::renderEcharts4r({
            most_recent_data_per_enrollment() |>
                dplyr::filter(domestic_violence_survivor == "Yes") |>
                dplyr::count(when_occurred, .drop = FALSE) |>
                bar_chart(
                    x = "when_occurred",
                    y = "n"
                )
        })

        output$currently_fleeing_chart <- echarts4r::renderEcharts4r({
            most_recent_data_per_enrollment() |>
                dplyr::filter(domestic_violence_survivor == "Yes") |>
                dplyr::count(currently_fleeing, .drop = FALSE) |>
                bar_chart(
                    x = "currently_fleeing",
                    y = "n"
                )
        })
    })
}

## To be copied in the UI
# mod_domestic_violence_ui("domestic_violence_1")

## To be copied in the server
# mod_domestic_violence_server("domestic_violence_1")
