#' parenting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parenting_ui <- function(id) {
    ns <- NS(id)
    tagList(
        bslib::layout_columns(
            mod_value_box_ui(
                id = ns("n_pregnant_heads_of_household_and_adults"),
                title = "# of Pregnant Head of Household and/or Adults"
            ),
            mod_value_box_ui(
                id = ns("n_households_with_children"),
                title = "# of Households with Children",
                tooltip = "Households with at least one participant enrolled as the head of household's child"
            ),
            mod_value_box_ui(
                id = ns("n_children"),
                title = "# of Children",
                tooltip = "A child is defined as a participant who is enrolled as the head of household's child"
            )
        ),
        bslib::layout_columns(
            custom_card(
                bslib::card_header(
                    with_popover(
                        text = "# of Children per Household",
                        content = "A child is defined as a participant who is enrolled as the head of household's child"
                    )
                ),
                echarts4r::echarts4rOutput(outputId = ns("n_children_per_household_chart"), height = "100%")
            )
        ),
        custom_card(
            bslib::card_header(
                with_popover(
                    text = "# of Head of Household and/or Adults by Pregnancy Status",
                    content = link_section("R10 Pregnancy Status")
                )
            ),
            echarts4r::echarts4rOutput(outputId = ns("pregnancy_chart"), height = "100%")
        )
    )
}

#' parenting Server Functions
#'
#' @noRd
mod_parenting_server <- function(id, health_data, enrollment_data, clients_filtered, heads_of_household_and_adults) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Filter Data ####
        health_data_filtered <- shiny::reactive(
            filter_data(health_data, clients_filtered()) |>
                dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
        )

        most_recent_data_per_enrollment <- shiny::reactive({
            health_data_filtered() |>
                dplyr::filter(data_collection_stage %in% c("Project start", "Project exit")) |>
                filter_most_recent_data_per_enrollment()
        })

        enrollment_data_filtered <- shiny::reactive(
            enrollment_data |>
                filter_data(clients_filtered())
        )

        household_data_filtered <- shiny::reactive({
            enrollment_data_filtered() |>
                dplyr::mutate(is_head_of_households_child = relationship_to_ho_h == "Head of household's child") |>
                dplyr::summarize(n_children = sum(is_head_of_households_child), .by = household_id)
        })

        # Value Boxes ####
        mod_value_box_server(
            id = "n_pregnant_heads_of_household_and_adults",
            rctv_data = shiny::reactive({
                most_recent_data_per_enrollment() |>
                    dplyr::filter(pregnancy_status == "Yes")
            })
        )

        mod_value_box_server(
            id = "n_households_with_children",
            rctv_data = shiny::reactive({
                household_data_filtered() |>
                    dplyr::filter(n_children > 0)
            })
        )

        mod_value_box_server(
            id = "n_children",
            rctv_data = shiny::reactive({
                enrollment_data_filtered() |>
                    dplyr::filter(relationship_to_ho_h == "Head of household's child")
            })
        )

        # Charts ####
        output$n_children_per_household_chart <- echarts4r::renderEcharts4r({
            household_data_filtered() |>
                dplyr::count(n_children, .drop = FALSE) |>
                dplyr::arrange(dplyr::desc(n_children)) |>
                dplyr::mutate(n_children = as.character(n_children)) |>
                bar_chart(
                    x = "n_children",
                    y = "n"
                )
        })

        output$pregnancy_chart <- echarts4r::renderEcharts4r({
            most_recent_data_per_enrollment() |>
                dplyr::count(pregnancy_status, .drop = FALSE) |>
                bar_chart(
                    x = "pregnancy_status",
                    y = "n"
                )
        })
    })
}

## To be copied in the UI
# mod_parenting_ui("parenting_1")

## To be copied in the server
# mod_parenting_server("parenting_1")
