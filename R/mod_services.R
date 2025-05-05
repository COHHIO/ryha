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
        bslib::card(
            shiny::dateRangeInput(
                inputId = ns("date_provided_filter"),
                label = "Date Provided",
                start = NULL,
                end = NULL,
                min = NULL,
                max = NULL
            )
        ),
        custom_card(
            height = "720px",
            bslib::card_header(
                with_popover(
                    text = "# of Youth by Service Type Provided",
                    content = link_section("R14 RHY Service Connections")
                )
            ),
            echarts4r::echarts4rOutput(outputId = ns("services_bar_chart"), height = "100%")
        ),
        custom_card(
            height = "720px",
            bslib::card_header(
                with_popover(
                    text = "# of Youth by Referral Source",
                    content = link_section("R1 Referral Source")
                )
            ),
            echarts4r::echarts4rOutput(outputId = ns("referral_bar_chart"), height = "100%")
        )
    )
}

#' services Server Functions
#'
#' @noRd
mod_services_server <- function(id, services_data, referral_data, clients_filtered) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        shiny::observe({
            shiny::req(
                nrow(services_data) >= 1L,
                any(!is.na(services_data$date_provided))
            )

            # Update the "Date Provided" date range filter
            shiny::updateDateRangeInput(
                session = session,
                inputId = "date_provided_filter",
                start = min(services_data$date_provided, na.rm = TRUE),
                end = max(services_data$date_provided, na.rm = TRUE),
                min = min(services_data$date_provided, na.rm = TRUE),
                max = max(services_data$date_provided, na.rm = TRUE)
            )
        })

        # Apply the filters to the services data
        services_data_filtered <- shiny::reactive(
            services_data |>
                dplyr::filter(
                    dplyr::between(
                        date_provided,
                        left = input$date_provided_filter[1],
                        right = input$date_provided_filter[2]
                    )
                ) |>
                dplyr::inner_join(
                    clients_filtered(),
                    by = c("personal_id", "organization_id", "enrollment_id")
                )
        )

        # Apply the filters to the referral data
        referral_data_filtered <- shiny::reactive({
            shiny::req(services_data_filtered())

            referral_data |>
                dplyr::inner_join(
                    services_data_filtered() |>
                        dplyr::select(personal_id, organization_id),
                    by = c("personal_id", "organization_id")
                ) |>
                dplyr::inner_join(
                    clients_filtered(),
                    by = c("personal_id", "organization_id", "enrollment_id")
                )
        })

        # Create reactive data frame to data to be displayed in bar chart
        services_bar_chart_data <- shiny::reactive({
            validate_data(services_data_filtered())

            services_data_filtered() |>
                dplyr::filter(!is.na(type_provided)) |>
                dplyr::distinct(personal_id, organization_id, type_provided) |>
                dplyr::count(type_provided) |>
                dplyr::arrange(n)
        })

        # Create services chart
        output$services_bar_chart <- echarts4r::renderEcharts4r({
            services_bar_chart_data() |>
                echarts4r::e_charts(x = type_provided) |>
                echarts4r::e_bar(
                    serie = n,
                    name = "# of Youth",
                    legend = FALSE,
                    label = list(
                        formatter = "{@[0]}",
                        show = TRUE,
                        position = "right"
                    )
                ) |>
                echarts4r::e_tooltip(trigger = "item") |>
                echarts4r::e_flip_coords() |>
                echarts4r::e_grid(containLabel = TRUE) |>
                echarts4r::e_show_loading()
        })

        # Create reactive data frame to data to be displayed in bar chart
        referral_bar_chart_data <- shiny::reactive({
            validate_data(referral_data_filtered())

            referral_data_filtered() |>
                dplyr::mutate(
                    referral_source = dplyr::case_when(
                        is.na(referral_source) ~ "(Blank)",
                        TRUE ~ referral_source
                    )
                ) |>
                dplyr::distinct(personal_id, organization_id, referral_source) |>
                dplyr::count(referral_source) |>
                dplyr::arrange(n)
        })

        # Create referral source chart
        output$referral_bar_chart <- echarts4r::renderEcharts4r({
            referral_bar_chart_data() |>
                echarts4r::e_charts(x = referral_source) |>
                echarts4r::e_bar(
                    serie = n,
                    name = "# of Youth",
                    legend = FALSE,
                    label = list(
                        formatter = "{@[0]}",
                        show = TRUE,
                        position = "right"
                    )
                ) |>
                echarts4r::e_tooltip(trigger = "item") |>
                echarts4r::e_flip_coords() |>
                echarts4r::e_grid(containLabel = TRUE) |>
                echarts4r::e_show_loading()
        })
    })
}

## To be copied in the UI
# mod_services_ui("services_1")

## To be copied in the server
# mod_services_server("services_1")
