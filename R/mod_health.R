#' health UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_health_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      bslib::card_header(shiny::h2("Health Status")),
      bslib::layout_columns(
        custom_card(
          bslib::card_header(
            with_popover(
              text = "# of Head of Household and/or Adults by Health Status",
              content = shiny::tagList(
                shiny::span("Each bar summarizes the responses for the corresponding health status."),
                shiny::br(),
                shiny::span("For more information, refer to sections:"),
                shiny::tags$ul(
                  shiny::tags$li(shiny::tags$b("R7 General Health Status")),
                  shiny::tags$li(shiny::tags$b("R8 Dental Health Status")),
                  shiny::tags$li(shiny::tags$b("R9 Mental Health Status"))
                ),
                shiny::span("in the ", link_data_standards_manual())
              )
            )
          ),
          echarts4r::echarts4rOutput(outputId = ns("health_status_chart"), height = "100%")
        ),
        custom_card(
          bslib::card_header(
            with_popover(
              text = "Changes in Health Status (Entry --> Exit)",
              content = shiny::tagList(
                shiny::span("For more information, refer to sections:"),
                shiny::tags$ul(
                  shiny::tags$li(shiny::tags$b("R7 General Health Status")),
                  shiny::tags$li(shiny::tags$b("R8 Dental Health Status")),
                  shiny::tags$li(shiny::tags$b("R9 Mental Health Status"))
                ),
                shiny::span("in the ", link_data_standards_manual())
              )
            )
          ),
          bslib::navset_card_tab(
            bslib::nav_panel(
              title = "General",
              echarts4r::echarts4rOutput(outputId = ns("general_sankey_chart"), height = "100%")
            ),
            bslib::nav_panel(
              title = "Dental",
              echarts4r::echarts4rOutput(outputId = ns("dental_sankey_chart"), height = "100%")
            ),
            bslib::nav_panel(
              title = "Mental",
              echarts4r::echarts4rOutput(outputId = ns("mental_sankey_chart"), height = "100%")
            )
          ) |>
            shiny::tagAppendAttributes(class = "nav-justified")
        )
      )
    ),
    bslib::card(
      bslib::card_header(shiny::h2("Counseling")),
      custom_card(
        bslib::card_header(
          with_popover(
            text = "# of Head of Household and/or Adults by Counseling Received Response",
            content = link_section("R18 Counseling")
          )
        ),
        echarts4r::echarts4rOutput(outputId = ns("counseling_chart"), height = "100%")
      )
    ),
  )
}

#' health Server Functions
#'
#' @noRd
mod_health_server <- function(id, health_data, counseling_data, clients_filtered, heads_of_household_and_adults){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Filter health data
    health_data_filtered <- shiny::reactive({
      filter_data(health_data, clients_filtered()) |>
        dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
    })

    # Create reactive with the most recent data collected per enrollment
    most_recent_health_data_per_enrollment <- shiny::reactive({
      health_data_filtered() |>
        # Health data should be collected only at Project start and Project exit
        dplyr::filter(data_collection_stage %in% c("Project start", "Project exit")) |>
        filter_most_recent_data_per_enrollment()
    })

    # Filter counseling data
    counseling_data_filtered <- shiny::reactive({
      filter_data(counseling_data, clients_filtered()) |>
        dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
    })

    # General Health ----
    output$health_status_chart <- echarts4r::renderEcharts4r({
      most_recent_health_data_per_enrollment() |>
        tidyr::pivot_longer(
          cols = c(
            general_health_status,
            dental_health_status,
            mental_health_status
          ),
          names_to = "health_type",
          values_to = "health_status"
        ) |>
        dplyr::mutate(
          health_type = health_type |>
            stringr::str_replace_all("_", " ") |>
            stringr::str_to_title() |> 
            factor(
              levels = c(
                "Mental Health Status",
                "Dental Health Status",
                "General Health Status"
              ),
              ordered = TRUE
            )
        ) |>
        dplyr::count(health_type, health_status, .drop = FALSE) |>
        dplyr::group_by(health_type) |>
        dplyr::mutate(pct = round(n / sum(n), 4)) |>
        dplyr::ungroup() |>
        # Order response categories
        dplyr::group_by(health_status) |> 
        # Create chart
        echarts4r::e_chart(x = health_type) |>
        echarts4r::e_bar(serie = n, stack = "my_stack") |>
        echarts4r::e_add_nested('extra', pct) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_grid(containLabel = TRUE) |>
        echarts4r::e_color(
          c(
           COLORS$EXCELLENT , # "Excellent",
           COLORS$VERY_GOOD , # "Very Good",
           COLORS$GOOD , # "Good",
           COLORS$FAIR , # "Fair",
           COLORS$POOR , # "Poor",
           COLORS$CLIENT_DOESNT_KNOW , # "Client doesn't know",
           COLORS$CLIENT_PREFERS_NOT_TO_ANSWER , # "Client prefers not to answer",
           COLORS$DATA_NOT_COLLECTED , # "Data not collected",
           COLORS$MISSING   # "Missing"
          )
        ) |>
        add_stacked_bar_tooltip()
    })

    # Create general health status sankey chart
    output$general_sankey_chart <- echarts4r::renderEcharts4r({
      health_data_filtered() |>
        prepare_sankey_data(
          response_col = "general_health_status",
          response_vals = c(
              "Excellent",
              "Very good",
              "Good",
              "Fair",
              "Poor"
            )
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

    # Create dental health status sankey chart
    output$dental_sankey_chart <- echarts4r::renderEcharts4r({
      health_data_filtered() |>
        prepare_sankey_data(
          response_col = "dental_health_status",
          response_vals = c(
              "Excellent",
              "Very good",
              "Good",
              "Fair",
              "Poor"
            )
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

    # Create mental health status sankey chart
    output$mental_sankey_chart <- echarts4r::renderEcharts4r({
      health_data_filtered() |>
        prepare_sankey_data(
          response_col = "mental_health_status",
          response_vals = c(
              "Excellent",
              "Very good",
              "Good",
              "Fair",
              "Poor"
            )
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

    # Counseling ----
    output$counseling_chart <- echarts4r::renderEcharts4r({
      counseling_data_filtered() |>
        dplyr::count(counseling_received, .drop = FALSE) |>
        bar_chart(
          x = "counseling_received",
          y = "n"
        )
    })

  })
}

## To be copied in the UI
# mod_health_ui("health_1")
## To be copied in the server
# mod_health_server("health_1")
