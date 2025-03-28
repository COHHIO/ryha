#' domestic_violence UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_domestic_violence_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidRow(

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth")),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user", class = "fa-solid"),
        width = 6
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_domestic_violence")),
        subtitle = "Total # of Youth with Domestic Violence Data Available",
        icon = shiny::icon("user-shield"),
        width = 6
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 12,

        bs4Dash::box(
          title = with_popover(
            text = "# of Head of Household and/or Adults by Domestic Violence Victim Response",
            content = link_section("4.11 Domestic Violence")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("victim_chart"),
            height = "100%"
          )
        )
      )

    ),

    shiny::fluidRow(

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = with_popover(
            text = "# of Domestic Violence Victims by When Occurred Response",
            content = link_section("4.11 Domestic Violence")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("when_occurred_chart"),
            height = "100%"
          )
        )

      ),

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = with_popover(
            text = "# of Domestic Violence Victims by Currently Fleeing Response",
            content = link_section("4.11 Domestic Violence")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("currently_fleeing_chart"),
            height = "100%"
          )
        )

      )
    )

  )
}

#' domestic_violence Server Functions
#'
#' @noRd
mod_domestic_violence_server <- function(id, domestic_violence_data, clients_filtered, heads_of_household_and_adults){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Filter domestic violence data
    domestic_violence_data_filtered <- shiny::reactive({
      filter_data(domestic_violence_data, clients_filtered()) |>
        dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
    })

    # Create reactive with the most recent data collected per enrollment
    most_recent_data_per_enrollment <- shiny::reactive({
      domestic_violence_data_filtered() |>
        # Domestic violence data is not expected to be collected at Project exit
        dplyr::filter(data_collection_stage != "Project exit") |>
        filter_most_recent_data_per_enrollment()
    })

    # Total number of Youth in program(s) that exist in the `domestic_violence.csv`
    # file
    n_youth_with_domestic_violence_data <- shiny::reactive(

      domestic_violence_data_filtered() |>
        dplyr::filter(
          domestic_violence_survivor %in% c("Yes", "No")
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    # Render number of youth w/ services box value
    output$n_youth_with_domestic_violence <- shiny::renderText({
      n_youth_with_domestic_violence_data()
    })

    # Create education pie chart
    output$victim_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |>
        dplyr::count(domestic_violence_survivor, .drop = FALSE) |>
        bar_chart(
          x = "domestic_violence_survivor",
          y = "n"
        )
    })

    output$when_occurred_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |>
        dplyr::filter(domestic_violence_survivor == "Yes") |> 
        dplyr::count(when_occurred, .drop = FALSE) |>
        bar_chart(
          x = "when_occurred",
          y = "n"
        )
    })

    output$currently_fleeing_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |>
        dplyr::filter(domestic_violence_survivor == "Yes") |> 
        dplyr::count(currently_fleeing, .drop = FALSE) |>
        bar_chart(
          x = "currently_fleeing",
          y = "n"
        )
    })

  })
}

## To be copied in the UI
# mod_domestic_violence_ui("domestic_violence_1")

## To be copied in the server
# mod_domestic_violence_server("domestic_violence_1")
