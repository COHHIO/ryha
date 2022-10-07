#' exits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_exits_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidRow(
      shiny::column(
        width = 12,
        echarts4r::echarts4rOutput(outputId = ns("exits_chart"))
      )
    )

  )
}

#' exits Server Functions
#'
#' @noRd
mod_exits_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    exits_df <- read_exit(
      file = "data/stage_for_db/hudx-111_YWCA/Exit.csv",
      submission_id = 3L
    )

    client_df <- read_client(
      file = "data/stage_for_db/hudx-111_YWCA/Client.csv",
      submission_id = 3L
    )

    chart_data <- client_df |>
      dplyr::select(PersonalID) |>
      dplyr::left_join(
        exits_df |> dplyr::select(PersonalID, Destination),
        by = "PersonalID"
      ) |>
      dplyr::count(Destination) |>
      dplyr::mutate(Destination = dplyr::if_else(
        is.na(Destination), "Did Not Exit", Destination
      )) |>
      dplyr::mutate(Start = "Entry")

    output$exits_chart <- echarts4r::renderEcharts4r(

      chart_data |>
        echarts4r::e_charts() |>
        echarts4r::e_sankey(Start, Destination, n) |>
        echarts4r::e_tooltip(trigger = "item")

    )

  })
}

## To be copied in the UI
# mod_exits_ui("exits_1")

## To be copied in the server
# mod_exits_server("exits_1")
