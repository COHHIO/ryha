#' disabilities UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_disabilities_ui <- function(id){
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
          outputId = ns("n_youth_with_disabilities_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 5,

        bs4Dash::box(
          title = "# of Disabled Youth by Disability Type",
          width = NULL,
          echarts4r::echarts4rOutput(
            outputId = ns("disabilities_pie_chart"),
            height = "600px"
          )
        )

      ),

      shiny::column(
        width = 7,

        bs4Dash::box(
          title = "Trend of Disability Types",
          width = NULL,
          echarts4r::echarts4rOutput(
            outputId = ns("disabilities_line_chart"),
            height = "600px"
          )
        )

      )

    )

  )
}

#' disabilities Server Functions
#'
#' @noRd
mod_disabilities_server <- function(id, filtered_dm){
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
    n_youth_with_disabilities_data <- shiny::reactive(

      filtered_dm()$disabilities |>
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
    output$n_youth_with_disabilities_data_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_disabilities_data(),
        subtitle = "Total # of Youth with Disabilities Data Available",
        icon = shiny::icon("home")
      )

    })

    output$disabilities_line_chart <- echarts4r::renderEcharts4r({

      filtered_dm()$disabilities |>
        dplyr::inner_join(
          filtered_dm()$submission |> dplyr::select(submission_id, quarter),
          by = "submission_id"
        ) |>
        dplyr::arrange(submission_id, personal_id, dplyr::desc(information_date)) |>
        dplyr::select(quarter, personal_id, disability_type, disability_response) |>
        dplyr::filter(disability_response == "Yes") |>
        dplyr::distinct(quarter, personal_id, disability_type) |>
        dplyr::count(quarter, disability_type) |>
        dplyr::group_by(disability_type) |>
        echarts4r::e_charts(x = quarter) |>
        echarts4r::e_line(serie = n) |>
        echarts4r::e_tooltip(trigger = "axis")

    })

    # TODO // Still need to filter this down to the most *recent* quarter's data
    # so that we are not duplicating individuals
    output$disabilities_pie_chart <- echarts4r::renderEcharts4r({

      filtered_dm()$disabilities |>
        dplyr::inner_join(
          filtered_dm()$submission |>
            dplyr::group_by(project_id) |>
            dplyr::filter(export_end_date == max(export_end_date)) |>
            dplyr::ungroup() |>
            dplyr::select(submission_id, project_id),
          by = "submission_id"
        ) |>
        dplyr::arrange(submission_id, personal_id, dplyr::desc(information_date)) |>
        dplyr::select(personal_id, disability_type, disability_response) |>
        dplyr::distinct(personal_id, disability_type, .keep_all = TRUE) |>
        dplyr::filter(disability_response == "Yes") |>
        dplyr::count(disability_type) |>
        dplyr::arrange(dplyr::desc(n)) |>
        echarts4r::e_chart(x = disability_type) |>
        echarts4r::e_pie(
          serie = n,
          name = "Disability Type",
          legend = TRUE,
          label = list(show = TRUE, position = "inside", formatter = "{c}"),
          radius = c("50%", "70%"),
          emphasis = list(label = list(show = TRUE, fontSize = "15", fontWeight = "bold"))
        ) |>
        echarts4r::e_tooltip(trigger = "item") |>
        echarts4r::e_grid(containLabel = TRUE)

    })

  })
}

## To be copied in the UI
# mod_disabilities_ui("disabilities_1")

## To be copied in the server
# mod_disabilities_server("disabilities_1")
