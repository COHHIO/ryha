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

    custom_card(
      bslib::card_header(
        with_popover(
          text = "# of Head of Household and/or Adults by Employment Status",
          content = link_section("R6 Employment Status")
        )
      ),
      echarts4r::echarts4rOutput(outputId = ns("employed_chart"), height = "100%")
    ),

    bslib::layout_columns(
      custom_card(
        bslib::card_header(
          with_popover(
            text = "# of Employed Youth by Employment Type",
            content = link_section("R6 Employment Status")
          )
        ),
        echarts4r::echarts4rOutput(outputId = ns("employment_type_chart"), height = "100%")
      ),
      custom_card(
        bslib::card_header(
          with_popover(
            text = "# of Not Employed Youth by Reason Not Employed",
            content = link_section("R6 Employment Status")
          )
        ),
        echarts4r::echarts4rOutput(outputId = ns("not_employed_reason_chart"), height = "100%")
      )
    ),

    custom_card(
      bslib::card_header(
        with_popover(
          text = "Changes in Employed Status (Entry --> Exit)",
          content = link_section("R6 Employment Status")
        )
      ),
      echarts4r::echarts4rOutput(outputId = ns("employed_sankey_chart"), height = "100%")
    )
  )
}

#' employment Server Functions
#'
#' @noRd
mod_employment_server <- function(id, employment_data, clients_filtered, heads_of_household_and_adults){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Filter employment data
    employment_data_filtered <- shiny::reactive({
      filter_data(employment_data, clients_filtered()) |>
        dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
    })

    # Create reactive with the most recent data collected per enrollment
    most_recent_data_per_enrollment <- shiny::reactive({
      employment_data_filtered() |>
        # Employment data should be collected only at Project start and Project exit
        dplyr::filter(data_collection_stage %in% c("Project start", "Project exit")) |>
        filter_most_recent_data_per_enrollment()
    })

    output$employed_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |> 
        dplyr::count(employed, .drop = FALSE) |>
        bar_chart(
          x = "employed",
          y = "n"
        )
    })

    output$employment_type_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |> 
        dplyr::filter(employed == "Yes") |>
        dplyr::count(employment_type, .drop = FALSE) |>
        bar_chart(
          x = "employment_type",
          y = "n"
        )
    })

    output$not_employed_reason_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |> 
        dplyr::filter(employed == "No") |>
        dplyr::count(not_employed_reason, .drop = FALSE) |>
        bar_chart(
          x = "not_employed_reason",
          y = "n"
        )
    })

    output$employed_sankey_chart <- echarts4r::renderEcharts4r({
      employment_data_filtered() |> 
        prepare_sankey_data(
          response_col = "employed",
          response_vals = c("Yes", "No")
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

  })
}

## To be copied in the UI
# mod_employment_ui("employment_1")

## To be copied in the server
# mod_employment_server("employment_1")
