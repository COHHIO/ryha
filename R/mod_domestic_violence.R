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

        bs4Dash::tabsetPanel(
          type = "pills",

          shiny::tabPanel(
            title = "Domestic Violence Victim",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Youth by Domestic Violence Victim Response",
                    content = link_section("4.11 Domestic Violence")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("victim_pie_chart"),
                    height = "100%"
                  )
                )

              ),

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = "Data Quality Statistics",
                  width = NULL,
                  maximizable = TRUE,
                  reactable::reactableOutput(
                    outputId = ns("victim_missingness_stats_tbl")
                  )
                )

              )

            )

          ),

          shiny::tabPanel(
            title = "When Occurred",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Youth by When Occurred Response",
                    content = link_section("4.11 Domestic Violence")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("when_occurred_pie_chart"),
                    height = "100%"
                  )
                )

              ),

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = "Data Quality Statistics",
                  width = NULL,
                  maximizable = TRUE,
                  reactable::reactableOutput(
                    outputId = ns("when_occurred_missingness_stats_tbl")
                  )
                )

              )

            )

          ),

          shiny::tabPanel(
            title = "Currently Fleeing",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Youth by Currently Fleeing Response",
                    content = link_section("4.11 Domestic Violence")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("currently_fleeing_pie_chart"),
                    height = "100%"
                  )
                )

              ),

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = "Data Quality Statistics",
                  width = NULL,
                  maximizable = TRUE,
                  reactable::reactableOutput(
                    outputId = ns("currently_fleeing_missingness_stats_tbl")
                  )
                )

              )

            )

          )

        )

      )

    )

  )
}

#' domestic_violence Server Functions
#'
#' @noRd
mod_domestic_violence_server <- function(id, domestic_violence_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Filter domestic violence data
    domestic_violence_data_filtered <- shiny::reactive({
      filter_data(domestic_violence_data, clients_filtered())
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
    output$victim_pie_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |>
        dplyr::filter(domestic_violence_survivor %in% c("Yes", "No")) |>
        dplyr::count(domestic_violence_survivor) |>
        pie_chart(
          category = "domestic_violence_survivor",
          count = "n"
        )
    })

    # Capture the data quality statistics for "domestic_violence_survivor" field
    victim_missingness_stats <- shiny::reactive({

      domestic_violence_data_filtered() |>
        dplyr::mutate(domestic_violence_survivor = ifelse(
          is.na(domestic_violence_survivor),
          "(Blank)",
          domestic_violence_survivor
        )) |>
        dplyr::filter(
          !domestic_violence_survivor %in% c("Yes", "No")
        ) |>
        dplyr::count(domestic_violence_survivor, name = "Count") |>
        dplyr::rename(Response = domestic_violence_survivor)

    })

    # Create the {reactable} table to hold the missingness stats
    output$victim_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        victim_missingness_stats()
      )
    )

    output$when_occurred_pie_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |>
        # Remove missing values
        dplyr::filter(
          !when_occurred %in% get_missing_categories(),
          !is.na(when_occurred)
        ) |>
        dplyr::count(when_occurred) |>
        pie_chart(
          category = "when_occurred",
          count = "n"
        )
    })

    # Capture the data quality statistics for "when_occurred" field
    when_occurred_missingness_stats <- shiny::reactive({

      domestic_violence_data_filtered() |>
        dplyr::mutate(when_occurred = ifelse(
          is.na(when_occurred),
          "(Blank)",
          when_occurred
        )) |>
        dplyr::filter(
          domestic_violence_survivor == "Yes",
          when_occurred %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(when_occurred, name = "Count") |>
        dplyr::rename(Response = when_occurred)

    })

    # Create the {reactable} table to hold the missingness stats
    output$when_occurred_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        when_occurred_missingness_stats()
      )
    )

    output$currently_fleeing_pie_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |>
        dplyr::filter(
          !currently_fleeing %in% get_missing_categories(),
          !is.na(currently_fleeing)
        ) |>
        dplyr::count(currently_fleeing) |>
        pie_chart(
          category = "currently_fleeing",
          count = "n"
        )
    })

    # Capture the data quality statistics for "currently_fleeing" field
    currently_fleeing_missingness_stats <- shiny::reactive({

      domestic_violence_data_filtered() |>
        dplyr::mutate(currently_fleeing = ifelse(
          is.na(currently_fleeing),
          "(Blank)",
          currently_fleeing
        )) |>
        dplyr::filter(
          domestic_violence_survivor == "Yes",
          currently_fleeing %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(currently_fleeing, name = "Count") |>
        dplyr::rename(Response = currently_fleeing)

    })

    # Create the {reactable} table to hold the missingness stats
    output$currently_fleeing_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        currently_fleeing_missingness_stats()
      )
    )

  })
}

## To be copied in the UI
# mod_domestic_violence_ui("domestic_violence_1")

## To be copied in the server
# mod_domestic_violence_server("domestic_violence_1")
