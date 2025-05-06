#' income_benefits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_income_benefits_ui <- function(id) {
    ns <- NS(id)
    tagList(
        bslib::card(
            bslib::navset_card_tab(
                bslib::nav_panel(
                    title = "Income",
                    bslib::layout_columns(
                        mod_value_box_ui(
                            id = ns("n_heads_of_household_and_adults_with_income_data"),
                            title = "# of Head of Household and/or Adults with Income Data",
                            tooltip = "Head of Household and/or Adults included in Overview who also appear in Income records"
                        ),
                        mod_value_box_ui(
                            id = ns("n_heads_of_household_and_adults_missing_income_data"),
                            title = "# of Head of Household and/or Adults Missing",
                            tooltip = "Head of Household and/or Adults included in Overview without a matching Income record"
                        ),
                    ),
                    bslib::layout_columns(
                        custom_card(
                            bslib::card_header(
                                with_popover(
                                    text = "# of Head of Household and/or Adults by Income Received (from Any Source) Response",
                                    content = link_section("4.02 Income and Sources")
                                )
                            ),
                            echarts4r::echarts4rOutput(outputId = ns("income_chart"), height = "100%")
                        ),
                        custom_card(
                            bslib::card_header(
                                with_popover(
                                    text = "Informed Income Sources",
                                    content = shiny::tagList(
                                        shiny::p("Only Head of Household and Adults who reported receiving income are included."),
                                        shiny::p("Each bar represents the percentage of youth who informed a given income source."),
                                        shiny::p("Since individuals can select multiple sources, the total percentage may exceed 100%."),
                                        shiny::p(link_section("4.02 Income and Sources"))
                                    )
                                )
                            ),
                            echarts4r::echarts4rOutput(outputId = ns("income_source_chart"), height = "100%")
                        )
                    ),
                    custom_card(
                        bslib::card_header(
                            with_popover(
                                text = "# of Head of Household and/or Adults with Income by Total Monthly Income",
                                content = link_section("4.02 Income and Sources")
                            )
                        ),
                        echarts4r::echarts4rOutput(outputId = ns("income_bar_chart"), height = "100%")
                    )
                ),
                bslib::nav_panel(
                    title = "Benefits",
                    bslib::layout_columns(
                        mod_value_box_ui(
                            id = ns("n_heads_of_household_and_adults_with_benefits_data"),
                            title = "# of Head of Household and/or Adults with Benefits Data",
                            tooltip = "Head of Household and/or Adults included in Overview who also appear in Benefits records"
                        ),
                        mod_value_box_ui(
                            id = ns("n_heads_of_household_and_adults_missing_benefits_data"),
                            title = "# of Head of Household and/or Adults Missing",
                            tooltip = "Head of Household and/or Adults included in Overview without a matching Benefits record"
                        )
                    ),
                    bslib::layout_columns(
                        custom_card(
                            bslib::card_header(
                                with_popover(
                                    text = "# of Head of Household and/or Adults by Benefits Received (from Any Source) Response",
                                    content = link_section("4.03 Non-Cash Benefits")
                                )
                            ),
                            echarts4r::echarts4rOutput(outputId = ns("benefits_chart"), height = "100%")
                        ),
                        custom_card(
                            bslib::card_header(
                                with_popover(
                                    text = "Informed Benefits Source",
                                    content = shiny::tagList(
                                        shiny::p("Only Head of Household and Adults who reported receiving benefits are included."),
                                        shiny::p("Each bar represents the percentage of youth who informed a given benefit source."),
                                        shiny::p("Since individuals can select multiple sources, the total percentage may exceed 100%."),
                                        shiny::p(link_section("4.03 Non-Cash Benefits"))
                                    )
                                )
                            ),
                            echarts4r::echarts4rOutput(outputId = ns("benefits_source_chart"), height = "100%")
                        )
                    ),
                    custom_card(
                        bslib::card_header(
                            with_popover(
                                text = "Changes in Benefits (from Any Source) Response (Entry --> Exit)",
                                content = link_section("4.03 Non-Cash Benefits")
                            )
                        ),
                        echarts4r::echarts4rOutput(outputId = ns("benefits_sankey_chart"), height = "100%")
                    )
                ),
                bslib::nav_panel(
                    title = "Health Insurance",
                    bslib::layout_columns(
                        mod_value_box_ui(
                            id = ns("n_youth_with_health_insurance_data"),
                            title = "# of Youth with Health Insurance Data",
                            tooltip = "Youth included in Overview who also appear in Health Insurance records"
                        ),
                        mod_value_box_ui(
                            id = ns("n_youth_missing_health_insurance_data"),
                            title = "# of Youth Missing",
                            tooltip = "Youth included in Overview without a matching Health Insurance record"
                        )
                    ),
                    bslib::layout_columns(
                        custom_card(
                            bslib::card_header(
                                with_popover(
                                    text = "# of Youth by Health Insurance Received (from Any Source) Response",
                                    content = link_section("4.04 Health Insurance")
                                )
                            ),
                            echarts4r::echarts4rOutput(outputId = ns("insurance_chart"), height = "100%")
                        ),
                        custom_card(
                            bslib::card_header(
                                with_popover(
                                    text = "Informed Health Insurance Source",
                                    content = shiny::tagList(
                                        shiny::p("Only youth who reported receiving health insurance are included."),
                                        shiny::p("Each bar represents the percentage of youth who informed a given health insurance source."),
                                        shiny::p("Since individuals can select multiple sources, the total percentage may exceed 100%."),
                                        shiny::p(link_section("4.04 Health Insurance"))
                                    )
                                )
                            ),
                            echarts4r::echarts4rOutput(outputId = ns("insurance_source_chart"), height = "100%")
                        )
                    ),
                    custom_card(
                        bslib::card_header(
                            with_popover(
                                text = "Changes in Health Insurance (from Any Source) Response (Entry --> Exit)",
                                content = link_section("4.04 Health Insurance")
                            )
                        ),
                        echarts4r::echarts4rOutput(outputId = ns("insurance_sankey_chart"), height = "100%")
                    )
                )
            ) |>
                shiny::tagAppendAttributes(class = "nav-justified")
        )
    )
}

#' income_benefits Server Functions
#'
#' @noRd
mod_income_benefits_server <- function(id, income_data, benefits_data, clients_filtered, heads_of_household_and_adults) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Filter Data ####
        ## Income ####
        income_data_filtered <- shiny::reactive({
            filter_data(income_data, clients_filtered()) |>
                dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
        })

        most_recent_income_data_per_enrollment <- shiny::reactive({
            income_data_filtered() |>
                filter_most_recent_data_per_enrollment()
        })

        ## Benefits ####
        benefits_data_filtered <- shiny::reactive({
            filter_data(benefits_data, clients_filtered())
        })

        most_recent_benefits_data_per_enrollment_hh_and_adults <- shiny::reactive({
            benefits_data_filtered() |>
                dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id")) |>
                filter_most_recent_data_per_enrollment()
        })

        most_recent_benefits_data_per_enrollment_all_clients <- shiny::reactive({
            benefits_data_filtered() |>
                filter_most_recent_data_per_enrollment()
        })

        # Value Boxes ####
        mod_value_box_server(
            id = "n_heads_of_household_and_adults_with_income_data",
            rctv_data = most_recent_income_data_per_enrollment
        )

        mod_value_box_server(
            id = "n_heads_of_household_and_adults_missing_income_data",
            rctv_data = shiny::reactive({
                filter_data(heads_of_household_and_adults, clients_filtered()) |>
                    dplyr::anti_join(
                        most_recent_income_data_per_enrollment(),
                        by = c("enrollment_id", "personal_id", "organization_id")
                    )
            })
        )

        mod_value_box_server(
            id = "n_heads_of_household_and_adults_with_benefits_data",
            rctv_data = most_recent_benefits_data_per_enrollment_hh_and_adults
        )

        mod_value_box_server(
            id = "n_heads_of_household_and_adults_missing_benefits_data",
            rctv_data = shiny::reactive({
                filter_data(heads_of_household_and_adults, clients_filtered()) |>
                    dplyr::anti_join(
                        most_recent_benefits_data_per_enrollment_hh_and_adults(),
                        by = c("enrollment_id", "personal_id", "organization_id")
                    )
            })
        )

        mod_value_box_server(
            id = "n_youth_with_health_insurance_data",
            rctv_data = most_recent_benefits_data_per_enrollment_all_clients
        )

        mod_value_box_server(
            id = "n_youth_missing_health_insurance_data",
            rctv_data = shiny::reactive({
                clients_filtered() |>
                    dplyr::anti_join(
                        most_recent_benefits_data_per_enrollment_all_clients(),
                        by = c("enrollment_id", "personal_id", "organization_id")
                    )
            })
        )

        # Charts ####
        ## Income ####
        output$income_chart <- echarts4r::renderEcharts4r(
            most_recent_income_data_per_enrollment() |>
                dplyr::count(income_from_any_source, .drop = FALSE) |>
                bar_chart(
                    x = "income_from_any_source",
                    y = "n"
                )
        )

        ## Income Source ####
        output$income_source_chart <- echarts4r::renderEcharts4r({
            youth_with_income_from_any_source <- most_recent_income_data_per_enrollment() |>
                dplyr::filter(income_from_any_source == "Yes")

            youth_with_income_from_any_source |>
                dplyr::select(
                    enrollment_id,
                    organization_id,
                    personal_id,
                    earned,
                    unemployment,
                    ssi,
                    ssdi,
                    va_disability_service,
                    va_disability_non_service,
                    private_disability,
                    workers_comp,
                    tanf,
                    ga,
                    soc_sec_retirement,
                    pension,
                    child_support,
                    alimony,
                    other_income_source
                ) |>
                tidyr::pivot_longer(
                    cols = earned:other_income_source,
                    names_to = "income_source",
                    values_to = "response"
                ) |>
                dplyr::filter(response == "Yes") |>
                dplyr::mutate(
                    income_source = factor(
                        income_source,
                        levels = c(
                            "other_income_source",
                            "alimony",
                            "child_support",
                            "pension",
                            "soc_sec_retirement",
                            "ga",
                            "tanf",
                            "workers_comp",
                            "private_disability",
                            "va_disability_non_service",
                            "va_disability_service",
                            "ssdi",
                            "ssi",
                            "unemployment",
                            "earned"
                        ),
                        labels = c(
                            "Other",
                            "Alimony",
                            "Child Support",
                            "Pension",
                            "Social Security Retirement",
                            "GA",
                            "TANF",
                            "Workers Comp",
                            "Private Disability",
                            "VA Disability Non Service",
                            "VA Disability Service",
                            "SSDI",
                            "SSI",
                            "Unemployment",
                            "Earned"
                        ),
                        ordered = TRUE
                    )
                ) |>
                dplyr::count(income_source, .drop = FALSE) |>
                bar_chart(
                    x = "income_source",
                    y = "n",
                    pct_denominator = nrow(youth_with_income_from_any_source)
                )
        })

        ## Monthly Income ####
        output$income_bar_chart <- echarts4r::renderEcharts4r({
            youth_with_income_from_any_source <- most_recent_income_data_per_enrollment() |>
                dplyr::filter(income_from_any_source == "Yes")

            youth_with_income_from_any_source |>
                dplyr::count(total_monthly_income_grouped, .drop = FALSE) |>
                bar_chart(
                    x = "total_monthly_income_grouped",
                    y = "n"
                )
        })

        ## Benefits ####
        output$benefits_chart <- echarts4r::renderEcharts4r({
            most_recent_benefits_data_per_enrollment_hh_and_adults() |>
                dplyr::count(benefits_from_any_source, .drop = FALSE) |>
                bar_chart(
                    x = "benefits_from_any_source",
                    y = "n"
                )
        })

        ## Benefits Source ####
        output$benefits_source_chart <- echarts4r::renderEcharts4r({
            youth_with_benefits_from_any_source <- most_recent_benefits_data_per_enrollment_hh_and_adults() |>
                dplyr::filter(benefits_from_any_source == "Yes")

            youth_with_benefits_from_any_source |>
                dplyr::select(
                    enrollment_id,
                    organization_id,
                    snap,
                    wic,
                    tanf_child_care,
                    tanf_transportation,
                    other_tanf,
                    other_benefits_source
                ) |>
                tidyr::pivot_longer(
                    cols = snap:other_benefits_source,
                    names_to = "benefits_source",
                    values_to = "response"
                ) |>
                dplyr::filter(response == "Yes") |>
                dplyr::mutate(
                    benefits_source = factor(
                        benefits_source,
                        levels = c(
                            "other_benefits_source",
                            "other_tanf",
                            "tanf_transportation",
                            "tanf_child_care",
                            "wic",
                            "snap"
                        ),
                        labels = c(
                            "Other",
                            "Other TANF",
                            "TANF Transportation",
                            "TANF Child Care",
                            "WIC",
                            "Snap"
                        ),
                        ordered = TRUE
                    )
                ) |>
                dplyr::count(benefits_source, .drop = FALSE) |>
                bar_chart(
                    x = "benefits_source",
                    y = "n",
                    pct_denominator = nrow(youth_with_benefits_from_any_source)
                )
        })

        ## Benefits Sankey ####
        output$benefits_sankey_chart <- echarts4r::renderEcharts4r(
            benefits_data_filtered() |>
                prepare_sankey_data(
                    response_col = "benefits_from_any_source",
                    response_vals = c("Yes", "No")
                ) |>
                sankey_chart(
                    entry_status = "Entry",
                    exit_status = "Exit",
                    count = "n"
                )
        )

        ## Health Insurance ####
        output$insurance_chart <- echarts4r::renderEcharts4r({
            most_recent_benefits_data_per_enrollment_all_clients() |>
                dplyr::count(insurance_from_any_source, .drop = FALSE) |>
                bar_chart(
                    x = "insurance_from_any_source",
                    y = "n"
                )
        })

        ## Health Insurance Source ####
        output$insurance_source_chart <- echarts4r::renderEcharts4r({
            youth_with_insurance_from_any_source <- most_recent_benefits_data_per_enrollment_all_clients() |>
                dplyr::filter(insurance_from_any_source == "Yes")

            youth_with_insurance_from_any_source |>
                dplyr::select(
                    enrollment_id,
                    organization_id,
                    personal_id,
                    medicaid,
                    medicare,
                    schip,
                    vha_services,
                    employer_provided,
                    cobra,
                    private_pay,
                    state_health_ins,
                    indian_health_services,
                    other_insurance
                ) |>
                tidyr::pivot_longer(
                    cols = medicaid:other_insurance,
                    names_to = "insurance_source",
                    values_to = "response"
                ) |>
                dplyr::filter(response == "Yes") |>
                dplyr::mutate(
                    insurance_source = factor(
                        insurance_source,
                        levels = c(
                            "other_insurance",
                            "indian_health_services",
                            "state_health_ins",
                            "private_pay",
                            "cobra",
                            "employer_provided",
                            "vha_services",
                            "schip",
                            "medicare",
                            "medicaid"
                        ),
                        labels = c(
                            "Other",
                            "Indian Health Services",
                            "State Health Insurance",
                            "Private Pay",
                            "Cobra",
                            "Employer Provided",
                            "VHA Services",
                            "SCHIP",
                            "Medicare",
                            "Medicaid"
                        ),
                        ordered = TRUE
                    )
                ) |>
                dplyr::count(insurance_source, .drop = FALSE) |>
                bar_chart(
                    x = "insurance_source",
                    y = "n",
                    pct_denominator = nrow(youth_with_insurance_from_any_source)
                )
        })

        ## Health Insurance Sankey ####
        output$insurance_sankey_chart <- echarts4r::renderEcharts4r(
            benefits_data_filtered() |>
                prepare_sankey_data(
                    response_col = "insurance_from_any_source",
                    response_vals = c("Yes", "No")
                ) |>
                sankey_chart(
                    entry_status = "Entry",
                    exit_status = "Exit",
                    count = "n"
                )
        )
    })
}

## To be copied in the UI
# mod_income_benefits_ui("income_benefits_1")

## To be copied in the server
# mod_income_benefits_server("income_benefits_1")
