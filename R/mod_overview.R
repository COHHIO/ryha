#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(
        width = 12,

        h3("Crosstable of All Project Youth Demographics"),

        echarts4r::echarts4rOutput(outputId = ns("heatmap"))

      )
    )

  )
}

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    heatmap_data <- reactive({

      prep_heatmap(data = data())

    })

    output$heatmap <- echarts4r::renderEcharts4r({

      heatmap_data() |>
        echarts4r::e_charts(name.x) |>
        echarts4r::e_heatmap(name.y, n, pointSize = 5) |>
        echarts4r::e_visual_map(
          serie = n,
          # itemWidth = 15,
          # show = "false"#,
          orient = "horizontal",
          left = "center",
          # bottom = "15%"
        ) |>
        echarts4r::e_tooltip(
          trigger = "item",
          formatter = htmlwidgets::JS("
            function(params){
              return('# of Youth Who Identify as' +
              '<br /><em>' + params.value[0] + '</em> & <em>' + params.value[1] + '</em>' +
              '<br />Count: <strong>' + params.value[2] + '</strong>')
            }
          ")
        ) |>
        echarts4r::e_x_axis(axisLabel = list(rotate = 45)) |>
        echarts4r::e_grid(bottom = "30%")

    })

  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
