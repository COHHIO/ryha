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

      shiny::column(
        width = 6,
        # Number of clients (post filters)
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_box"),
          width = "100%"
        )
      ),

      shiny::column(
        width = 6,
        # Number of projects (post filters)
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_health_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 4,
        bs4Dash::box(
          title = "# of Youth by Health Status",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("health_status_bar_chart"),
            height = "600px"
          )
        )
      ),

      shiny::column(
        width = 8,

        bs4Dash::tabsetPanel(
          type = "pills",

          shiny::tabPanel(
            title = "General",
            shiny::p("Placeholder")
            # bs4Dash::box(
            #   title = "Trend of Disability Types",
            #   width = NULL,
            #   maximizable = TRUE,
            #   echarts4r::echarts4rOutput(
            #     outputId = ns("last_grade_completed_line_chart"),
            #     height = "600px"
            #   )
            # )
          ),

          shiny::tabPanel(
            title = "Dental",
            shiny::p("Placeholder")
          ),

          shiny::tabPanel(
            title = "Mental",
            shiny::p("Placeholder")
          )

        )

      )
    )

  )
}

#' health Server Functions
#'
#' @noRd
mod_health_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # echarts4r code for stacked bar chart:
    output$health_status_bar_chart <- echarts4r::renderEcharts4r({

      read_health(
        file = "data/stage_for_db/hudx-111_1651250747/HealthAndDV.csv",
        submission_id = 1L
      )|>
        dplyr::select(GeneralHealthStatus:MentalHealthStatus) |>
        tidyr::drop_na() |>
        tidyr::pivot_longer(cols = dplyr::everything()) |>
        dplyr::count(name, value) |>
        dplyr::mutate(name = stringr::str_replace(
          string = name,
          pattern = "HealthStatus",
          replacement = ""
        )) |>
        echarts4r::group_by(value) |>
        echarts4r::e_charts(x = name) |>
        echarts4r::e_bar(serie = n, stack = "grp") |>
        echarts4r::e_tooltip(trigger = "item")

    })

  })
}

## To be copied in the UI
# mod_health_ui("health_1")
## To be copied in the server
# mod_health_server("health_1")
