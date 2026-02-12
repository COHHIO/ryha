#' services UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_services_ui <- function(id) {
    ns <- NS(id)
    tagList(
        bslib::layout_columns(
            mod_value_box_ui(
                id = ns("n_head_of_household_and_adults_with_services"),
                title = "Head of Household and/or Adults that were provided one or more services"
            ),
            mod_value_box_ui(
                id = ns("n_different_services_provided"),
                title = "Different services provided"
            ),
            mod_value_box_ui(
                id = ns("n_head_of_household_and_adults_without_services"),
                title = "Head of Household and/or Adults that were provided no services"
            )
        ),
        custom_card(
            height = "680px",
            bslib::card_header(
                with_popover(
                    text = "Head of Household and/or Adults by Service Type Provided",
                    content = shiny::tagList(
                        shiny::p("Each bar represents the percentage of participants that were provided the corresponding service throughout the enrollment."),
                        shiny::p("Since participants can be provided multiple services, the total percentage may exceed 100%."),
                        shiny::p(link_section("R14 RHY Service Connections"))
                    )
                )
            ),
            echarts4r::echarts4rOutput(outputId = ns("services_bar_chart"), height = "100%")
        ),
        custom_card(
            bslib::card_header(
                with_popover(
                    text = "Head of Household and/or Adults by Number of Services Provided",
                    content = link_section("R14 RHY Service Connections")
                )
            ),
            echarts4r::echarts4rOutput(outputId = ns("services_count_bar_chart"), height = "100%")
        )
    )
}

#' services Server Functions
#'
#' @noRd
mod_services_server <- function(id, services_data, clients_filtered, heads_of_household_and_adults_filtered) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Filter Data ####
        services_data_filtered <- shiny::reactive({
            filter_data(services_data, clients_filtered()) |>
                dplyr::semi_join(heads_of_household_and_adults_filtered(), by = c("enrollment_id", "personal_id", "organization_id")) |>
                # Remove duplicate service records within each enrollment
                dplyr::distinct(enrollment_id, personal_id, organization_id, type_provided)
        })

        heads_of_household_and_adults_with_services <- shiny::reactive({
            services_data_filtered() |>
                dplyr::distinct(enrollment_id, personal_id, organization_id)
        })

        heads_of_household_and_adults_without_services <- shiny::reactive({
            filter_data(heads_of_household_and_adults_filtered(), clients_filtered()) |>
                dplyr::anti_join(
                    heads_of_household_and_adults_with_services(),
                    by = c("enrollment_id", "personal_id", "organization_id")
                )
        })

        # Value Boxes ####
        mod_value_box_server(
            id = "n_head_of_household_and_adults_with_services",
            rctv_data = heads_of_household_and_adults_with_services
        )

        mod_value_box_server(
            id = "n_different_services_provided",
            rctv_data = shiny::reactive({
                services_data_filtered() |>
                    dplyr::distinct(type_provided)
            })
        )

        mod_value_box_server(
            id = "n_head_of_household_and_adults_without_services",
            rctv_data = heads_of_household_and_adults_without_services
        )

        # Create services chart
        output$services_bar_chart <- echarts4r::renderEcharts4r({
            services_data_filtered() |>
                dplyr::count(type_provided, .drop = FALSE) |>
                bar_chart(
                    x = "type_provided",
                    y = "n",
                    pct_denominator = nrow(heads_of_household_and_adults_filtered())
                )
        })

        # Create services count chart
        output$services_count_bar_chart <- echarts4r::renderEcharts4r({
            services_data_filtered() |>
                dplyr::count(enrollment_id, personal_id, organization_id) |>
                dplyr::mutate(
                    services_provided_count = dplyr::case_when(
                        n == 1 ~ "1 Service",
                        n <= 4 ~ paste(n, "Services"),
                        n <= 9 ~ "5 to 9 Services",
                        n >= 10 ~ "10+ Services"
                    ) |>
                        factor(
                            levels = c(
                                "No Services",
                                "1 Service",
                                paste(c("2", "3", "4"), "Services"),
                                "5 to 9 Services",
                                "10+ Services"
                            ),
                            ordered = TRUE
                        )
                ) |>
                dplyr::count(services_provided_count, .drop = FALSE) |>
                dplyr::mutate(
                    n = dplyr::case_when(
                        services_provided_count == "No Services" ~ nrow(heads_of_household_and_adults_without_services()),
                        TRUE ~ n
                    )
                ) |>
                bar_chart(
                    x = "services_provided_count",
                    y = "n",
                    serie_name = "# of Participants that were provided"
                )
        })
    })
}

## To be copied in the UI
# mod_services_ui("services_1")

## To be copied in the server
# mod_services_server("services_1")
