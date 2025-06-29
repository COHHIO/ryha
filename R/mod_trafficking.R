#' trafficking UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_trafficking_ui <- function(id) {
    ns <- NS(id)
    tagList(
        bslib::layout_columns(
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_with_records"),
                title = "Head of Household and/or Adults with Records",
                tooltip = "Responses within those records may still be missing"
            ),
            mod_value_box_ui(
                id = ns("n_sex_trafficked_heads_of_household_and_adults"),
                title = "Sex Trafficked Head of Household and/or Adults"
            ),
            mod_value_box_ui(
                id = ns("n_labor_trafficked_heads_of_household_and_adults"),
                title = "Head of Household and/or Adults that were Labor Trafficked",
                tooltip = shiny::HTML("
                  A Head of Household and/or Adult is considered to have been labor trafficked if either:
                  <ul>
                    <li>They were ever afraid to quit or leave work due to threats of violence to themselves, their family, or friends</li>
                    <li>They were ever promised work where the work or payment was different than expected</li>
                  </ul>
                "),
                tooltip_options = list(customClass = "tooltip-wider")
            )
        ),
        bslib::navset_card_tab(
            bslib::nav_panel(
                title = "Sex Trafficking",
                custom_card(
                    bslib::card_header(
                        with_popover(
                            text = "Head of Household and/or Adults by Exchange for Sex Response",
                            content = link_section("R15 Commercial Sexual Exploitation/Sex Trafficking")
                        )
                    ),
                    echarts4r::echarts4rOutput(outputId = ns("exchange_sex_chart"), height = "100%")
                ),
                bslib::layout_columns(
                    custom_card(
                        bslib::card_header(
                            with_popover(
                                text = "Head of Household and/or Adults by Asked or Forced to Exchange Response",
                                content = shiny::tagList(
                                    shiny::p("Only participants that ever received anything in exchange for sex are included."),
                                    shiny::p(link_section("R15 Commercial Sexual Exploitation/Sex Trafficking"))
                                )
                            )
                        ),
                        echarts4r::echarts4rOutput(outputId = ns("asked_sex_chart"), height = "100%")
                    ),
                    custom_card(
                        bslib::card_header(
                            with_popover(
                                text = "Head of Household and/or Adults by Count of Exchange for Sex Response",
                                content = shiny::tagList(
                                    shiny::p("Only participants that ever received anything in exchange for sex are included."),
                                    shiny::p(link_section("R15 Commercial Sexual Exploitation/Sex Trafficking"))
                                )
                            )
                        ),
                        echarts4r::echarts4rOutput(outputId = ns("count_sex_chart"), height = "100%")
                    )
                )
            ),
            bslib::nav_panel(
                title = "Labor Trafficking",
                bslib::layout_columns(
                    custom_card(
                        bslib::card_header(
                            with_popover(
                                text = "Head of Household and/or Adults by Workplace Violence/Threats Response",
                                content = link_section("R16 Labor Exploitation/Trafficking")
                            )
                        ),
                        echarts4r::echarts4rOutput(outputId = ns("violence_labor_chart"), height = "100%")
                    ),
                    custom_card(
                        bslib::card_header(
                            with_popover(
                                text = "Head of Household and/or Adults by Workplace Promise Difference Response",
                                content = link_section("R16 Labor Exploitation/Trafficking")
                            )
                        ),
                        echarts4r::echarts4rOutput(outputId = ns("promise_labor_chart"), height = "100%")
                    )
                ),
                custom_card(
                    bslib::card_header(
                        with_popover(
                            text = "Head of Household and/or Adults by Coerced to Continue Work Response",
                            content = shiny::tagList(
                                shiny::p("Only Head of Household and Adults that experienced workplace violence and/or promise difference are included."),
                                shiny::p(link_section("R16 Labor Exploitation/Trafficking"))
                            )
                        )
                    ),
                    echarts4r::echarts4rOutput(outputId = ns("coerced_labor_chart"), height = "100%")
                )
            )
        ) |>
            shiny::tagAppendAttributes(class = "nav-justified")
    )
}

#' trafficking Server Functions
#'
#' @noRd
mod_trafficking_server <- function(id, trafficking_data, clients_filtered, heads_of_household_and_adults) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Filter Data ####
        trafficking_data_filtered <- shiny::reactive(
            filter_data(trafficking_data, clients_filtered()) |>
                dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
        )

        # Value Boxes ####
        mod_value_box_server(
            id = "n_heads_of_household_and_adults_with_records",
            rctv_data = trafficking_data_filtered
        )

        mod_value_box_server(
            id = "n_sex_trafficked_heads_of_household_and_adults",
            rctv_data = shiny::reactive({
                trafficking_data_filtered() |>
                    dplyr::filter(exchange_for_sex == "Yes")
            })
        )

        mod_value_box_server(
            id = "n_labor_trafficked_heads_of_household_and_adults",
            rctv_data = shiny::reactive({
                trafficking_data_filtered() |>
                    dplyr::filter(
                        dplyr::if_any(
                            .cols = c(
                                work_place_violence_threats,
                                workplace_promise_difference
                            ),
                            .fns = function(col) col == "Yes"
                        )
                    )
            })
        )

        # Charts ####
        ## Sex Trafficking ####
        ### Exchange for Sex ####
        output$exchange_sex_chart <- echarts4r::renderEcharts4r({
            trafficking_data_filtered() |>
                dplyr::count(exchange_for_sex, .drop = FALSE) |>
                bar_chart(
                    x = "exchange_for_sex",
                    y = "n"
                )
        })

        ### Count of Exchange for Sex ####
        output$count_sex_chart <- echarts4r::renderEcharts4r({
            trafficking_data_filtered() |>
                dplyr::filter(exchange_for_sex == "Yes") |>
                dplyr::count(count_of_exchange_for_sex, .drop = FALSE) |>
                bar_chart(
                    x = "count_of_exchange_for_sex",
                    y = "n"
                )
        })

        ### Asked or Forced to Exchange for Sex ####
        output$asked_sex_chart <- echarts4r::renderEcharts4r(
            trafficking_data_filtered() |>
                dplyr::filter(exchange_for_sex == "Yes") |>
                dplyr::count(asked_or_forced_to_exchange_for_sex, .drop = FALSE) |>
                bar_chart(
                    x = "asked_or_forced_to_exchange_for_sex",
                    y = "n"
                )
        )

        ## Labor Trafficking ####
        ### Workplace Violence Threats ####
        output$violence_labor_chart <- echarts4r::renderEcharts4r(
            trafficking_data_filtered() |>
                dplyr::count(work_place_violence_threats, .drop = FALSE) |>
                bar_chart(
                    x = "work_place_violence_threats",
                    y = "n"
                )
        )

        ### Workplace Promise Difference ####
        output$promise_labor_chart <- echarts4r::renderEcharts4r(
            trafficking_data_filtered() |>
                dplyr::count(workplace_promise_difference, .drop = FALSE) |>
                bar_chart(
                    x = "workplace_promise_difference",
                    y = "n"
                )
        )

        ### Coerced to Continue Work ####
        output$coerced_labor_chart <- echarts4r::renderEcharts4r({
            trafficking_data_filtered() |>
                dplyr::filter(work_place_violence_threats == "Yes" | workplace_promise_difference == "Yes") |>
                dplyr::count(coerced_to_continue_work, .drop = FALSE) |>
                bar_chart(
                    x = "coerced_to_continue_work",
                    y = "n"
                )
        })
    })
}

## To be copied in the UI
# mod_trafficking_ui("trafficking_1")

## To be copied in the server
# mod_trafficking_server("trafficking_1")
