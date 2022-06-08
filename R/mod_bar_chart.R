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
        width = 3,

        shiny::selectInput(
          inputId = ns("group_by_overview"),
          label = "Group By:",
          choices = c("Ethnicity", "Gender", "Veteran Status"),
          selected = "Ethnicity"
        )

      ),

      column(
        width = 3,

        shiny::selectInput(
          inputId = ns("gender_filter_overview"),
          label = "Gender:",
          choices = c("Male", "Female", "Transgender", "NoSingleGender"),
          selected = c("Male", "Female", "Transgender", "NoSingleGender"),
          multiple = TRUE
        )

      ),

      column(
        width = 3,

        shiny::selectInput(
          inputId = ns("veteran_filter_overview"),
          label = "Veteran Status:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        )

      )
    ),

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

    # bar_chart_data <- reactive(
    #   prep_bar_chart(
    #     data = data,
    #     group = input$group_by_overview
    #   )
    # )

    output$bar_chart <- echarts4r::renderEcharts4r({

      Sys.sleep(3)




      req(input$group_by_overview)

      selected_table <- tolower(input$group_by_overview)

      # message(selected_table)

      generate_bar_chart(
        data = data |>
          purrr::pluck(selected_table),
        group = input$group_by_overview
      ) |>
        echarts4r::e_show_loading()

    })

  })
}

## To be copied in the UI
# mod_bar_chart_ui("bar_chart_1")

## To be copied in the server
# mod_bar_chart_server("bar_chart_1")
