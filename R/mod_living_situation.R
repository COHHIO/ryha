#' living_situation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_living_situation_ui <- function(id) {
    ns <- NS(id)
    tagList(
        bslib::layout_columns(
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_with_living_situation_data"),
                title = "# of Head of Household and/or Adults with Living Situation Data",
                tooltip = "Head of Household and/or Adults included in Overview who also appear in Living Situation records"
            ),
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_missing"),
                title = "# of Head of Household and/or Adults Missing",
                tooltip = "Head of Household and/or Adults included in Overview without a matching Living Situation record"
            ),
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_with_destination_data"),
                title = "# of Head of Household and/or Adults with Destination Data",
                tooltip = "Head of Household and/or Adults included in Overview who also appear in Destination records"
            ),
            mod_value_box_ui(
                id = ns("n_heads_of_household_and_adults_who_exited_to_permanent_housing"),
                title = "# of Head of Household and/or Adults that Exited to Permanent Housing"
            )
        ),
        bslib::layout_columns(
            custom_card(
                bslib::card_header(
                    with_popover(
                        text = "# of Head of Household and/or Adults by Living Situation Group (at Entry)",
                        content = shiny::tagList(
                            shiny::span("Response categories have been grouped to improve chart readability."),
                            shiny::br(),
                            link_section("3.917 Prior Living Situation")
                        )
                    )
                ),
                echarts4r::echarts4rOutput(outputId = ns("living_situation_chart"), height = "100%")
            ),
            custom_card(
                bslib::card_header(
                    with_popover(
                        text = "# of Head of Household and/or Adults by Destination Group (at Exit)",
                        content = shiny::tagList(
                            shiny::span("Response categories have been grouped to improve chart readability."),
                            shiny::br(),
                            link_section("3.12 Destination")
                        )
                    )
                ),
                echarts4r::echarts4rOutput(outputId = ns("destination_chart"), height = "100%")
            ),
        ),
        custom_card(
            height = "720px",
            bslib::card_header("# of Head of Household and/or Adults by Destination (at Exit)"),
            echarts4r::echarts4rOutput(outputId = ns("destination_bar_chart"), height = "100%")
        ),
        custom_card(
            bslib::card_header(
                with_popover(
                    text = "Changes in General Living Situation Group (Entry --> Exit)",
                    content = shiny::tagList(
                        shiny::span("Response categories have been grouped to improve chart readability.")
                    )
                )
            ),
            echarts4r::echarts4rOutput(outputId = ns("sankey_chart"), height = "100%")
        )
    )
}

#' living_situation Server Functions
#'
#' @noRd
mod_living_situation_server <- function(id, enrollment_data, exit_data, clients_filtered, heads_of_household_and_adults) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Filter Data ####
        living_data_filtered <- shiny::reactive({
            filter_data(enrollment_data, clients_filtered()) |>
                # Add destination data
                dplyr::left_join(
                    exit_data |>
                        dplyr::select(
                            enrollment_id,
                            personal_id,
                            organization_id,
                            exit_date,
                            destination,
                            destination_grouped
                        ),
                    by = c("enrollment_id", "personal_id", "organization_id")
                ) |>
                # Prior living situation is collected for head of household and adults.
                # Destination is collected for all youth.
                # Head of household and adults filter is applied to both for reporting consistency reasons
                dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
        })

        destination_chart_data <- shiny::reactive({
            living_data_filtered() |>
                dplyr::filter(!is.na(exit_date))
        })

        # Value Boxes ####
        mod_value_box_server(
            id = "n_heads_of_household_and_adults_with_living_situation_data",
            rctv_data = living_data_filtered
        )

        mod_value_box_server(
            id = "n_heads_of_household_and_adults_missing",
            rctv_data = shiny::reactive({
                filter_data(heads_of_household_and_adults, clients_filtered()) |>
                    dplyr::anti_join(
                        living_data_filtered(),
                        by = c("enrollment_id", "personal_id", "organization_id")
                    )
            })
        )

        mod_value_box_server(
            id = "n_heads_of_household_and_adults_with_destination_data",
            rctv_data = destination_chart_data
        )

        mod_value_box_server(
            id = "n_heads_of_household_and_adults_who_exited_to_permanent_housing",
            rctv_data = shiny::reactive({
                destination_chart_data() |>
                    dplyr::filter(destination %in% (LivingCodes |> dplyr::filter(ExitCategory == "Permanent") |> dplyr::pull(Description)))
            })
        )

        # Charts ####
        ## Living Situation (Grouped) ####
        output$living_situation_chart <- echarts4r::renderEcharts4r({
            living_data_filtered() |>
                dplyr::count(living_situation_grouped, .drop = FALSE) |>
                bar_chart(
                    x = "living_situation_grouped",
                    y = "n"
                )
        })

        ## Destination (Grouped) ####
        output$destination_bar_chart <- echarts4r::renderEcharts4r({
            destination_chart_data() |>
                # Not using ".drop = FALSE" to avoid displaying zero-count responses, as the variable has many response categories.
                dplyr::count(destination) |>
                dplyr::mutate(
                    # Assign destination group
                    destination_group = dplyr::case_when(
                        destination %in% (LivingCodes |> dplyr::filter(ExitCategory == "Homeless") |> dplyr::pull(Description)) ~ "Homeless",
                        destination %in% (LivingCodes |> dplyr::filter(ExitCategory == "Institutional") |> dplyr::pull(Description)) ~ "Institutional",
                        destination %in% (LivingCodes |> dplyr::filter(ExitCategory == "Temporary") |> dplyr::pull(Description)) ~ "Temporary",
                        destination %in% (LivingCodes |> dplyr::filter(ExitCategory == "Permanent") |> dplyr::pull(Description)) ~ "Permanent",
                        TRUE ~ "Other"
                    ) |>
                        factor(
                            levels = c(
                                "Other",
                                "Homeless",
                                "Institutional",
                                "Temporary",
                                "Permanent"
                            )
                        ),
                    # Assign color by destination_grouped
                    color = dplyr::case_when(
                        destination_group == "Other" ~ palette$missing,
                        destination_group == "Homeless" ~ palette$poor,
                        destination_group == "Institutional" ~ palette$fair,
                        destination_group == "Temporary" ~ palette$good,
                        destination_group == "Permanent" ~ palette$excellent
                    )
                ) |>
                # Sort by counts inside each group
                dplyr::arrange(destination_group, n) |>
                # Workaround to add legend by group
                dplyr::mutate(
                    Other = NA,
                    Homeless = NA,
                    Institutional = NA,
                    Temporary = NA,
                    Permanent = NA
                ) |>
                bar_chart(
                    x = "destination",
                    y = "n",
                    tooltip_opts = list(
                        confine = TRUE,
                        extraCssText = "width:auto; white-space:pre-wrap;"
                    )
                ) |>
                echarts4r::e_y_axis(
                    axisLabel = list(
                        width = 350,
                        overflow = "truncate"
                    )
                ) |>
                echarts4r::e_line(serie = Permanent) |>
                echarts4r::e_line(serie = Temporary) |>
                echarts4r::e_line(serie = Institutional) |>
                echarts4r::e_line(serie = Homeless) |>
                echarts4r::e_line(serie = Other) |>
                echarts4r::e_legend(
                    icon = "rect",
                    selectedMode = FALSE
                ) |>
                echarts4r::e_color(
                    c(
                        "",
                        palette$excellent,
                        palette$good,
                        palette$fair,
                        palette$poor,
                        palette$missing
                    )
                )
        })

        ## Destination ####
        output$destination_chart <- echarts4r::renderEcharts4r({
            destination_chart_data() |>
                dplyr::count(destination_grouped, .drop = FALSE) |>
                bar_chart(
                    x = "destination_grouped",
                    y = "n"
                )
        })

        ## Sankey ####
        output$sankey_chart <- echarts4r::renderEcharts4r({
            living_data_filtered() |>
                # Pivot longer to match expected data format
                tidyr::pivot_longer(
                    cols = c(living_situation_grouped, destination_grouped),
                    names_to = "data_collection_stage",
                    values_to = "living_situation_response"
                ) |>
                # Recode column accordingly
                dplyr::mutate(
                    data_collection_stage = dplyr::recode(
                        data_collection_stage,
                        "living_situation_grouped" = "Project start",
                        "destination_grouped" = "Project exit"
                    )
                ) |>
                prepare_sankey_data(
                    response_col = "living_situation_response",
                    response_vals = c(
                        "Homeless",
                        "Institutional",
                        "Temporary",
                        "Permanent"
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
# mod_living_situation_ui("living_situation_1")

## To be copied in the server
# mod_living_situation_server("living_situation_1")
