#' employment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_employment_ui <- function(id){
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
          outputId = ns("n_youth_with_employment_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 4,

        bs4Dash::box(
          title = "# of Youth by Employment Status",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("employment_pie_chart"),
            height = "600px"
          )
        )

      ),

      shiny::column(
        width = 8,

        bs4Dash::tabsetPanel(
          type = "pills",

          shiny::tabPanel(
            title = "Employed",

            bs4Dash::box(
              title = "Trend of Employed Type",
              width = NULL,
              maximizable = TRUE,
              echarts4r::echarts4rOutput(
                outputId = ns("employed_line_chart"),
                height = "560px"
              )
            )

          ),

          shiny::tabPanel(
            title = "Unemployed",

            bs4Dash::box(
              title = "Trend of Unemployed Reason",
              width = NULL,
              maximizable = TRUE,
              echarts4r::echarts4rOutput(
                outputId = ns("unemployed_line_chart"),
                height = "560px"
              )
            )

          )

        )
      )

    )

  )
}

#' employment Server Functions
#'
#' @noRd
mod_employment_server <- function(id, filtered_dm){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      filtered_dm()$client |>
        dplyr::inner_join(
          filtered_dm()$submission |> dplyr::select(submission_id, project_id),
          by = "submission_id"
        ) |>
        dplyr::distinct(project_id, personal_id) |>
        nrow()

    })

    # Total number of Youth in program(s) that exist in the `disabilities.csv`
    # file
    n_youth_with_employment_data <- shiny::reactive(

      filtered_dm()$employment |>
        dplyr::filter(employed %in% c("Yes", "No")) |>
        dplyr::inner_join(
          filtered_dm()$submission |> dplyr::select(submission_id, project_id),
          by = "submission_id"
        ) |>
        dplyr::distinct(project_id, personal_id) |>
        nrow()

    )

    # Render number of clients box
    output$n_youth_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth(),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user")
      )

    })

    # Render number of projects box
    output$n_youth_with_employment_data_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_employment_data(),
        subtitle = "Total # of Youth with Employment Data Available",
        icon = shiny::icon("home")
      )

    })

    # Create reactive data frame to data to be displayed in pie chart
    pie_chart_data <- shiny::reactive({

      filtered_dm()$employment |>
        dplyr::inner_join(
          filtered_dm()$submission |>
            dplyr::group_by(project_id) |>
            dplyr::filter(export_end_date == max(export_end_date)) |>
            dplyr::ungroup() |>
            dplyr::select(submission_id, project_id),
          by = "submission_id"
        ) |>
        dplyr::arrange(submission_id, personal_id, dplyr::desc(information_date)) |>
        dplyr::select(personal_id, employed) |>
        dplyr::distinct(personal_id, employed, .keep_all = TRUE) |>
        dplyr::count(employed) |>
        dplyr::arrange(employed) |>
        tidyr::drop_na()

    })

    # Create employment pie chart
    output$employment_pie_chart <- echarts4r::renderEcharts4r({

      pie_chart_data() |>
        echarts4r::e_chart(x = employed) |>
        echarts4r::e_pie(
          serie = n,
          name = "Employed Status",
          legend = TRUE,
          label = list(
            show = TRUE,
            position = "inside",
            formatter = "{c}"   # show the numeric value as the label
          ),
          radius = c("50%", "70%"),
          # emphasize the label when hovered over
          emphasis = list(
            label = list(
              show = TRUE,
              fontSize = "15",
              fontWeight = "bold"
            )
          )
        ) |>
        echarts4r::e_legend(bottom = 0) |>   # place legend below chart
        echarts4r::e_title(
          subtext = "Chart represents most recent quarter's data for each program selected"
        ) |>
        echarts4r::e_tooltip(trigger = "item") |>
        echarts4r::e_grid(containLabel = TRUE) |>
        echarts4r::e_show_loading()

    })

    # Create reactive data frame to data to be displayed in line chart
    employed_line_chart_data <- shiny::reactive({

      filtered_dm()$employment |>
        dplyr::filter(employed == "Yes") |>
        dplyr::mutate(
          employment_type = dplyr::if_else(
            is.na(employment_type),
            "Not provided",
            employment_type
          )
        ) |>
        dplyr::inner_join(
          filtered_dm()$submission |> dplyr::select(submission_id, quarter),
          by = "submission_id"
        ) |>
        dplyr::arrange(submission_id, personal_id, dplyr::desc(information_date)) |>
        dplyr::select(quarter, personal_id, employment_type) |>
        dplyr::distinct(quarter, personal_id, employment_type) |>
        dplyr::count(quarter, employment_type) |>
        dplyr::arrange(employment_type) |>
        dplyr::group_by(employment_type)

    })

    # Create employed trend line chart
    output$employed_line_chart <- echarts4r::renderEcharts4r({

      employed_line_chart_data() |>
        echarts4r::e_charts(x = quarter) |>
        echarts4r::e_line(serie = n, symbol = "circle") |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(top = "10%") |>
        echarts4r::e_show_loading()

    })

    # Create reactive data frame to data to be displayed in line chart
    unemployed_line_chart_data <- shiny::reactive({

      filtered_dm()$employment |>
        dplyr::filter(employed == "No") |>
        dplyr::mutate(
          not_employed_reason = dplyr::if_else(
            is.na(not_employed_reason),
            "Not provided",
            not_employed_reason
          )
        ) |>
        dplyr::inner_join(
          filtered_dm()$submission |> dplyr::select(submission_id, quarter),
          by = "submission_id"
        ) |>
        dplyr::arrange(submission_id, personal_id, dplyr::desc(information_date)) |>
        dplyr::select(quarter, personal_id, not_employed_reason) |>
        dplyr::distinct(quarter, personal_id, not_employed_reason) |>
        dplyr::count(quarter, not_employed_reason) |>
        dplyr::arrange(not_employed_reason) |>
        dplyr::group_by(not_employed_reason)

    })

    # Create unemployed trend line chart
    output$unemployed_line_chart <- echarts4r::renderEcharts4r({

      unemployed_line_chart_data() |>
        echarts4r::e_charts(x = quarter) |>
        echarts4r::e_line(serie = n, symbol = "circle") |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(top = "10%") |>
        echarts4r::e_show_loading()

    })

  })
}

## To be copied in the UI
# mod_employment_ui("employment_1")

## To be copied in the server
# mod_employment_server("employment_1")
