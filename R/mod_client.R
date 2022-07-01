#' client UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_client_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::column(
        width = 2,
        shiny::selectInput(
          inputId = ns("quarter"),
          label = "Submission",
          choices = NULL,
          width = "100%"
        )
      ),
      shiny::column(
        width = 5,
        bs4Dash::bs4ValueBoxOutput(outputId = ns("n_clients"), width = "100%")
      ),
      shiny::column(
        width = 5,
        bs4Dash::bs4ValueBoxOutput(outputId = ns("n_projects"), width = "100%")
      )
    ),
    shiny::fluidRow(
      echarts4r::echarts4rOutput(outputId = ns("chart_gender"), width = "50%", height = 350),
      echarts4r::echarts4rOutput(outputId = ns("chart_ethnicity"), width = "50%", height = 350)
    ),
    shiny::fluidRow(
      echarts4r::echarts4rOutput(outputId = ns("chart_age"), width = "50%", height = 350),
      echarts4r::echarts4rOutput(outputId = ns("chart_veteran"), width = "50%", height = 350)
    )
  )
}

#' client Server Functions
#'
#' @noRd
mod_client_server <- function(id, filtered_dm){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    list_quarters <- reactive({
      filtered_dm()$table_submission |>
        dplyr::pull(quarter) |>
        unique() |>
        sort(decreasing = TRUE)
    })

    observeEvent(list_quarters(), {
      shiny::updateSelectInput(
        inputId = "quarter",
        choices = list_quarters()
      )
    })

    filtered_dm_submission <- reactive({
      filtered_dm() |>
        dm::dm_filter(table_submission,
                      quarter == input$quarter) |>
        dm::dm_apply_filters()
    })

    n_clients <- shiny::reactive({
      filtered_dm_submission()$table_client |>
        nrow()
    })

    n_projects <- shiny::reactive({
      filtered_dm_submission()$table_submission |>
        nrow()
    })

    output$n_clients <- bs4Dash::renderbs4ValueBox({
      bs4Dash::bs4ValueBox(
        value = n_clients(),
        subtitle = "Total Clients",
        icon = shiny::icon("user")
      )
    })

    output$n_projects <- bs4Dash::renderbs4ValueBox({
      bs4Dash::bs4ValueBox(
        value = n_projects(),
        subtitle = "Total Projects",
        icon = shiny::icon("folder-open")
      )
    })


    output$chart_gender <- echarts4r::renderEcharts4r({
      filtered_dm_submission()$table_client |>
        dplyr::count(gender) |>
        dplyr::arrange(dplyr::desc(n)) |>
        echarts4r::e_chart(x = gender) |>
        echarts4r::e_bar(n, legend = FALSE, name = "Clients") |>
        echarts4r::e_labels(position = "right") |>
        echarts4r::e_tooltip() |>
        echarts4r::e_title("Clients by Gender") |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_y_axis(splitLine = list(show = FALSE)) |>
        echarts4r::e_x_axis(show = FALSE) |>
        echarts4r::e_grid(left = "20%")
    })

    output$chart_ethnicity <- echarts4r::renderEcharts4r({
      filtered_dm_submission()$table_client |>
        dplyr::mutate(ethnicity = stringr::str_wrap(ethnicity, 25)) |>
        dplyr::count(ethnicity) |>
        dplyr::arrange(dplyr::desc(n)) |>
        echarts4r::e_chart(x = ethnicity) |>
        echarts4r::e_bar(n, legend = FALSE, name = "Clients") |>
        echarts4r::e_labels(position = "right") |>
        echarts4r::e_tooltip() |>
        echarts4r::e_title("Clients by Ethnicity") |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_y_axis(splitLine = list(show = FALSE)) |>
        echarts4r::e_x_axis(show = FALSE) |>
        echarts4r::e_grid(left = "20%")
    })

    output$chart_age <- echarts4r::renderEcharts4r({
      filtered_dm_submission()$table_client |>
        echarts4r::e_chart() |>
        echarts4r::e_histogram(age, legend = FALSE, name = "Clients") |>
        echarts4r::e_tooltip() |>
        echarts4r::e_title("Clients by Age") |>
        echarts4r::e_y_axis(splitLine = list(show = FALSE))
    })

    output$chart_veteran <- echarts4r::renderEcharts4r({
      filtered_dm_submission()$table_client |>
        dplyr::count(veteran_status) |>
        dplyr::arrange(dplyr::desc(n)) |>
        echarts4r::e_chart(x = veteran_status) |>
        echarts4r::e_pie(n, legend = FALSE, name = "Is Veteran") |>
        echarts4r::e_title("Clients by Veteran Status") |>
        echarts4r::e_tooltip()
    })

  })
}

## To be copied in the UI
# mod_client_ui("client_1")

## To be copied in the server
# mod_client_server("client_1")
