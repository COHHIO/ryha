#' bar_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bar_chart_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(
        width = 12,

        echarts4r::echarts4rOutput(outputId = ns("bar_chart"))

      )
    )


  )
}

#' bar_chart Server Functions
#'
#' @noRd
mod_bar_chart_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    bar_chart_data <- reactive(prep_bar_chart(data()))

    output$bar_chart <- echarts4r::renderEcharts4r({

      bar_chart_data() |>
      echarts4r::e_chart(x = Ethnicity) |>
        echarts4r::e_bar(
          serie = Count,
          name = "# of Youth",
          legend = FALSE,
          label = list(
            formatter = '{@[0]}',
            show = TRUE,
            position = "right"
          )
        ) |>
        echarts4r::e_flip_coords()

    })

  })
}

## To be copied in the UI
# mod_bar_chart_ui("bar_chart_1")

## To be copied in the server
# mod_bar_chart_server("bar_chart_1")
