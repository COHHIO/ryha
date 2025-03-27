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

    shiny::fluidRow(

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth")),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user", class = "fa-solid"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_health")),
        subtitle = "Total # of Youth with Health Data Available",
        icon = shiny::icon("stethoscope"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_counseling")),
        subtitle = "Total # of Youth with Counseling Data Available",
        icon = shiny::icon("stethoscope"),
        width = 4
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 12,

        bs4Dash::tabsetPanel(
          type = "pills",

          shiny::tabPanel(
            title = "Health Status",

            shiny::fluidRow(

              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = with_popover(
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
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("health_status_chart"),
                    height = "100%"
                  )
                )

              )

            ),

            shiny::fluidRow(

              shiny::column(
                width = 12,

                bs4Dash::tabBox(
                  title = with_popover(
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
                  ),
                  type = "tabs",
                  side = "right",
                  height = DEFAULT_BOX_HEIGHT,
                  width = NULL,
                  maximizable = TRUE,

                  shiny::tabPanel(
                    title = "General",
                    echarts4r::echarts4rOutput(
                      outputId = ns("general_sankey_chart"),
                      height = "100%"
                    )
                  ),

                  shiny::tabPanel(
                    title = "Dental",
                    echarts4r::echarts4rOutput(
                      outputId = ns("dental_sankey_chart"),
                      height = "100%"
                    )
                  ),

                  shiny::tabPanel(
                    title = "Mental",
                    echarts4r::echarts4rOutput(
                      outputId = ns("mental_sankey_chart"),
                      height = "100%"
                    )
                  )

                ) |>
                  shiny::tagAppendAttributes(class = "sankey-tabset")
              )
            )

          ),

          # Counseling ----

          shiny::tabPanel(
            title = "Counseling",

            shiny::fluidRow(

              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Head of Household and/or Adults by Counseling Received Response",
                    content = link_section("R18 Counseling")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("counseling_chart"),
                    height = "100%"
                  )
                )

              )

            )

          )

        )

      )

    )

  )
}

#' health Server Functions
#'
#' @noRd
mod_health_server <- function(id, health_data, counseling_data, clients_filtered, heads_of_household_and_adults){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

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

    # Total number of Youth in with health data available
    n_youth_with_health_data <- shiny::reactive(

      health_data_filtered() |>
        dplyr::filter(
          general_health_status %in% HealthStatusCodes$Description[1:5] |
            dental_health_status %in% HealthStatusCodes$Description[1:5] |
            mental_health_status %in% HealthStatusCodes$Description[1:5]
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Total number of Youth in with counseling data available
    n_youth_with_counseling_data <- shiny::reactive(

      counseling_data_filtered() |>
        dplyr::filter(
          counseling_received %in% c("Yes", "No")
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    # Render number of projects box
    output$n_youth_with_health <- shiny::renderText({
      n_youth_with_health_data()
    })

    # Render number of projects box
    output$n_youth_with_counseling <- shiny::renderText({
      n_youth_with_counseling_data()
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
