#' living_situation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_living_situation_ui <- function(id){
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
        value = shiny::textOutput(outputId = ns("n_youth_with_living_situation_data")),
        subtitle = "Total # of Youth with Living Situation Data Available",
        icon = shiny::icon("bed"),
        width = 6
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = with_popover(
            text = "# of Youth by Living Situation Group (at Entry)",
            content = shiny::tagList(
              shiny::span("Response categories have been grouped to improve chart readability."),
              shiny::br(),
              link_section("3.917 Prior Living Situation")
            )
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("living_situation_pie_chart"),
            height = "100%"
          )
        )

      ),

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = with_popover(
            text = "# of Youth by Destination Group (at Exit)",
            content = shiny::tagList(
              shiny::span("Response categories have been grouped to improve chart readability."),
              shiny::br(),
              link_section("3.12 Destination")
            )
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("destination_pie_chart"),
            height = "100%"
          )
        )

      )

    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        bs4Dash::box(
          title = with_popover(
            text = "Changes in General Living Situation Group (Entry --> Exit)",
            content = shiny::tagList(
              shiny::span("Response categories have been grouped to improve chart readability.")
            )
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("sankey_chart"),
            height = "100%"
          )
        )

      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        bs4Dash::box(
          title = "# of Youth by Destination (at Exit)",
          width = NULL,
          height = "680px",
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("destination_bar_chart"),
            height = "100%"
          )
        )

      )
    ),

    bs4Dash::tabsetPanel(
      type = "pills",

      ## Benefits Tab Panel ----
      shiny::tabPanel(
        title = "Living Situation (Entry)",

        shiny::fluidRow(
          shiny::column(
            width = 12,

            bs4Dash::box(
              title = "Data Quality Statistics - Living Situation (Entry)",
              width = NULL,
              maximizable = TRUE,
              reactable::reactableOutput(
                outputId = ns("living_situation_missingness_stats_tbl")
              )
            )

          )
        )

      ),

      shiny::tabPanel(
        title = "Destination (Exit)",

        shiny::fluidRow(
          shiny::column(
            width = 12,

            bs4Dash::box(
              title = "Data Quality Statistics - Destination (Exit)",
              width = NULL,
              maximizable = TRUE,
              reactable::reactableOutput(
                outputId = ns("destination_missingness_stats_tbl")
              )
            )

          )
        )

      )

    )

  )
}

#' living_situation Server Functions
#'
#' @noRd
mod_living_situation_server <- function(id, enrollment_data, exit_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the enrollment data
    living_data_filtered <- shiny::reactive({
      filter_data(enrollment_data, clients_filtered()) |>
        # Add destination data
        dplyr::left_join(
          exit_data |>
            dplyr::select(
              enrollment_id,
              personal_id,
              organization_id,
              destination,
              destination_grouped
            ),
          by = c("enrollment_id", "personal_id", "organization_id")
        )
    })

    # Total number of Youth in program(s) that exist in the `employment.csv`
    # file
    n_youth_with_living_data <- shiny::reactive(

      living_data_filtered() |>
        dplyr::filter(
          !is.na(living_situation),
          !living_situation %in% c(
            "No exit interview completed",
            "Worker unable to determine",
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          )
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    # Render number of youth with living situation data box value
    output$n_youth_with_living_situation_data <- shiny::renderText({
      n_youth_with_living_data()
    })

    # Living Situation Pie Chart ----
    output$living_situation_pie_chart <- echarts4r::renderEcharts4r({
      living_data_filtered() |>
        dplyr::count(living_situation_grouped) |>
        dplyr::filter(!is.na(living_situation_grouped)) |>
        pie_chart(
          category = "living_situation_grouped",
          count = "n"
        )
    })

    destination_chart_data <- shiny::reactive({
      living_data_filtered() |>
        dplyr::count(destination, destination_grouped) |>
        # Remove missing data
        dplyr::filter(
          !destination %in% c("No exit interview completed",
                              "Worker unable to determine",
                              "Client doesn't know",
                              "Data not collected",
                              "Client prefers not to answer"),
          !is.na(destination)
        )
    })

    output$destination_bar_chart <- echarts4r::renderEcharts4r({
       destination_chart_data() |>
        dplyr::arrange(n) |>
        bar_chart(
          x = "destination",
          y = "n"
        ) |>
        echarts4r::e_y_axis(
          axisLabel = list(
            width = 350,
            overflow = "truncate"
          )
        ) |>
        echarts4r::e_tooltip(
          confine = TRUE,
          extraCssText = "width:auto; white-space:pre-wrap;"
        )
    })

    # Destination Pie Chart ----
    output$destination_pie_chart <- echarts4r::renderEcharts4r({
      destination_chart_data() |>
        dplyr::count(destination_grouped, wt = n) |>
        pie_chart(
          category = "destination_grouped",
          count = "n"
        )
    })

    # Sankey Chart ----
    output$sankey_chart <- echarts4r::renderEcharts4r({
      living_data_filtered() |>
        # Pivot longer to match expected data format
        tidyr::pivot_longer(
          cols = c(living_situation_grouped, destination_grouped),
          names_to = "data_collection_stage",
          values_to = "living_situation_response"
        ) |>
        # Recode column accordingly
        dplyr::mutate(
          data_collection_stage = dplyr::recode(
            data_collection_stage,
            "living_situation_grouped" = "Project start",
            "destination_grouped" = "Project exit"
          )
        ) |> 
        prepare_sankey_data(
          response_col = "living_situation_response",
          response_vals = c(
            "Homeless",
            "Institutional",
            "Temporary",
            "Permanent"
          )
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

    # Data Quality Stats Tables ----

    ## Living Situation Data Quality ----
    living_situation_missingness_stats <- shiny::reactive({

      living_data_filtered() |>
        dplyr::mutate(living_situation = ifelse(
          is.na(living_situation),
          "(Blank)",
          living_situation
        )) |>
        dplyr::filter(
          living_situation %in% c(
            "No exit interview completed",
            "Worker unable to determine",
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(living_situation, name = "Count") |>
        dplyr::rename(Response = living_situation)

    })

    output$living_situation_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        living_situation_missingness_stats()
      )
    )

    ## Destination Data Quality ----
    destination_missingness_stats <- shiny::reactive({

      living_data_filtered() |>
        dplyr::mutate(destination = ifelse(
          is.na(destination),
          "(Blank)",
          destination
        )) |>
        dplyr::filter(
          destination %in% c(
            "No exit interview completed",
            "Worker unable to determine",
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(destination, name = "Count") |>
        dplyr::rename(Response = destination)

    })

    output$destination_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        destination_missingness_stats()
      )
    )

  })
}

## To be copied in the UI
# mod_living_situation_ui("living_situation_1")

## To be copied in the server
# mod_living_situation_server("living_situation_1")
