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

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth")),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user", class = "fa-solid"),
        width = 6
      ),

      # Number of projects (post filters)
      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_employment")),
        subtitle = "Total # of Youth with Employment Data Available",
        icon = shiny::icon("briefcase"),
        width = 6
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 12,
        bs4Dash::box(
           title = with_tooltip(
             text = "# of Head of Household and/or Adults by Employment Status",
             content = link_section("R6 Employment Status")
           ),
           width = NULL,
           height = DEFAULT_BOX_HEIGHT,
           maximizable = TRUE,
           echarts4r::echarts4rOutput(
             outputId = ns("employed_chart"),
             height = "100%"
           )
         )
      )
    ),

    shiny::fluidRow(

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = with_tooltip(
            text = "# of Employed Youth by Employment Type",
            content = link_section("R6 Employment Status")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("employment_type_chart"),
            height = "100%"
          )
        )

      ),

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = with_tooltip(
            text = "# of Not Employed Youth by Reason Not Employed",
            content = link_section("R6 Employment Status")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("not_employed_reason_chart"),
            height = "100%"
          )
        )

      ),

    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        bs4Dash::box(
          title = with_tooltip(
            text = "Changes in Employed Status (Entry --> Exit)",
            content = link_section("R6 Employment Status")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("employed_sankey_chart"),
            height = "100%"
          )
        )

      )
    )

  )
}

#' employment Server Functions
#'
#' @noRd
mod_employment_server <- function(id, employment_data, clients_filtered, heads_of_household_and_adults){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

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

    # Total number of Youth in program(s) that exist in the `employment.csv`
    # file
    n_youth_with_employment_data <- shiny::reactive({
      employment_data_filtered() |>
        dplyr::filter(employed %in% c("Yes", "No")) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()
    })

    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    # Render number of projects box value
    output$n_youth_with_employment <- shiny::renderText({
      n_youth_with_employment_data()
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
