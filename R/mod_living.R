#' living UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_living_ui <- function(id){
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
          outputId = ns("n_youth_with_living_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 4,

        bs4Dash::box(
          title = "# of Youth by Current Living Situation",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("living_pie_chart"),
            height = "600px"
          )
        )

      ),

      shiny::column(
        width = 8,

        bs4Dash::box(
          title = "Trend of Current Living Situation",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("living_line_chart"),
            height = "600px"
          )
        )

      )

    )

  )
}

#' living Server Functions
#'
#' @noRd
mod_living_server <- function(id, filtered_dm){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # filtered_living_data <- shiny::reactive({
    #
    #   if (input$dedup_status) {
    #
    #     out <- filtered_dm() |>
    #       # ensure only valid SSNs
    #       dm::dm_filter(client, ssn_data_quality == "Full SSN reported") |>
    #       dm::
    #
    #       dplyr::filter(ssn_data_quality == "Full SSN reported")
    #     dplyr::distinct()
    #
    #   } else {
    #
    #
    #
    #   }
    #
    # })

    # TODO // This assumes that the same individual is not in two different
    # projects/grantees (i.e., not de-duplicating across projects)
    n_youth <- shiny::reactive({

      filtered_dm()$client |>
        dplyr::inner_join(
          filtered_dm()$submission |> dplyr::select(submission_id, project_id),
          by = "submission_id"
        ) |>
        dplyr::distinct(project_id, personal_id) |>
        nrow()

    })

    n_youth_with_living_data <- shiny::reactive(

      filtered_dm()$current_living_situation |>
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
    output$n_youth_with_living_data_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_living_data(),
        subtitle = "Total # of Youth with Current Living Situation Data Available",
        icon = shiny::icon("home")
      )

    })

    # Create reactive data frame to data to be displayed in pie chart
    pie_chart_data <- shiny::reactive({

      filtered_dm()$current_living_situation |>
        dplyr::inner_join(
          filtered_dm()$submission |>
            dplyr::group_by(project_id) |>
            dplyr::filter(export_end_date == max(export_end_date)) |>
            dplyr::ungroup() |>
            dplyr::select(submission_id, project_id),
          by = "submission_id"
        ) |>
        dplyr::arrange(submission_id, personal_id, dplyr::desc(information_date)) |>
        dplyr::distinct(personal_id, current_living_situation) |>
        dplyr::count(current_living_situation) |>
        dplyr::arrange(current_living_situation)

    })

    # Create current living situation pie chart
    output$living_pie_chart <- echarts4r::renderEcharts4r({

      pie_chart_data() |>
        echarts4r::e_charts(x = current_living_situation) |>
        echarts4r::e_pie(
          serie = n,
          name = "Current Living Situation",
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
        echarts4r::e_legend(
          bottom = 0,   # place legend below chart
          type = "scroll"
        ) |>
        echarts4r::e_title(
          subtext = "Chart represents most recent quarter's data for each program selected"
        ) |>
        echarts4r::e_tooltip(trigger = "item") |>
        echarts4r::e_grid(containLabel = TRUE) |>
        echarts4r::e_show_loading()

    })

    # Create reactive data frame to data to be displayed in line chart
    line_chart_data <- shiny::reactive({

      filtered_dm()$current_living_situation |>
        dplyr::inner_join(
          filtered_dm()$submission |> dplyr::select(submission_id, quarter),
          by = "submission_id"
        ) |>
        dplyr::arrange(submission_id, personal_id, dplyr::desc(information_date)) |>
        dplyr::select(quarter, personal_id, current_living_situation) |>
        dplyr::distinct(quarter, personal_id, .keep_all = TRUE) |>
        dplyr::count(quarter, current_living_situation) |>
        dplyr::arrange(current_living_situation) |>
        dplyr::group_by(current_living_situation)

    })

    # Create current living situation trend line chart
    output$living_line_chart <- echarts4r::renderEcharts4r({

      line_chart_data() |>
        echarts4r::e_charts(x = quarter) |>
        echarts4r::e_line(serie = n, symbol = "circle") |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(top = "20%") |>
        echarts4r::e_show_loading()

    })

  })
}

## To be copied in the UI
# mod_living_ui("living_1")

## To be copied in the server
# mod_living_server("living_1")
