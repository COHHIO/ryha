#' exit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_exit_ui <- function(id) {
    ns <- NS(id)
    tagList(
        mod_value_box_ui(
            id = ns("n_heads_of_household_and_adults_with_records"),
            title = "Head of Household and/or Adults with Records",
            tooltip = "Responses within those records may still be missing"
        ),
        bslib::card(
            bslib::card_header(shiny::h2("Project Completion Status")),
            custom_card(
                bslib::card_header(
                    with_popover(
                        text = "Head of Household and/or Adults by Project Completion Status",
                        content = link_section("R17 Project Completion Status")
                    )
                ),
                echarts4r::echarts4rOutput(outputId = ns("completion_chart"), height = "100%")
            )
        ),
        bslib::card(
            bslib::card_header(shiny::h2("Safe & Appropriate Exit")),
            custom_card(
                bslib::card_header(
                    with_popover(
                        text = "Head of Household and/or Adults by Safe & Appropriate Exit Response",
                        content = link_section("R19 Safe and Appropriate Exit")
                    )
                ),
                echarts4r::echarts4rOutput(outputId = ns("exit_heatmap"), height = "100%")
            )
        )
    )
}

#' exit Server Functions
#'
#' @noRd
mod_exit_server <- function(id, exit_data, clients_filtered, heads_of_household_and_adults) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Filter Data ####
        exit_data_filtered <- shiny::reactive({
            filter_data(exit_data, clients_filtered()) |>
                dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
        })

        # Value Boxes ####
        mod_value_box_server(
            id = "n_heads_of_household_and_adults_with_records",
            rctv_data = exit_data_filtered
        )

        # Charts ####
        output$completion_chart <- echarts4r::renderEcharts4r({
            exit_data_filtered() |>
                dplyr::count(project_completion_status, .drop = FALSE) |>
                bar_chart(
                    x = "project_completion_status",
                    y = "n"
                )
        })

        output$exit_heatmap <- echarts4r::renderEcharts4r({
            exit_data_filtered() |>
                dplyr::count(destination_safe_client, destination_safe_worker, .drop = FALSE) |>
                echarts4r::e_charts(
                    x = destination_safe_client,
                    label = list(show = TRUE, fontSize = 16)
                ) |>
                echarts4r::e_heatmap(
                    y = destination_safe_worker,
                    z = n
                ) |>
                echarts4r::e_visual_map(
                    serie = n,
                    show = FALSE
                ) |>
                echarts4r::e_axis_labels(
                    x = "Participant Response",
                    y = "Worker Response"
                ) |>
                echarts4r::e_x_axis(
                    nameLocation = "middle",
                    nameGap = 60,
                    nameTextStyle = list(fontSize = 14),
                    axisLabel = list(
                        interval = 0,
                        width = 110,
                        overflow = "break"
                    )
                ) |>
                echarts4r::e_y_axis(
                    nameLocation = "middle",
                    nameGap = 160,
                    nameTextStyle = list(fontSize = 14)
                ) |>
                echarts4r::e_tooltip(
                    trigger = "item",
                    formatter = htmlwidgets::JS("
                      function(params){
                          return('<strong>Participant Response: </strong>' + params.value[0] + '<br />' +
                                  '<strong>Worker Response: </strong>' + params.value[1] + '<br />' +
                                  params.marker + 'Participants: ' + params.value[2]
                          )
                      }
                    ")
                ) |>
                echarts4r::e_grid(containLabel = TRUE) |>
                echarts4r::e_show_loading()
        })
    })
}

## To be copied in the UI
# mod_exit_ui("exit_1")

## To be copied in the server
# mod_exit_server("exit_1")
