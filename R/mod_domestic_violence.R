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

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "Changes in Domestic Violence Victim Response (Entry --> Exit)",
                    content = link_section("4.11 Domestic Violence")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("victim_sankey_chart"),
                    height = "100%"
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

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "Changes in Currently Fleeing (Entry --> Exit)",
                    content = link_section("4.11 Domestic Violence")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("currently_fleeing_sankey_chart"),
                    height = "100%"
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

    # Apply the filters to the domestic_violence data
    domestic_violence_data_filtered <- shiny::reactive({
      filter_data(domestic_violence_data, clients_filtered())
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

    # Create reactive data frame to data to be displayed in pie chart
    victim_pie_chart_data <- shiny::reactive({

      out <- domestic_violence_data_filtered() |>
        dplyr::filter(
          domestic_violence_survivor %in% c("Yes", "No")
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          domestic_violence_survivor,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          domestic_violence_survivor
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          domestic_violence_survivor,
          .keep_all = TRUE
        )

      validate_data(out)

      out |>
        dplyr::count(domestic_violence_survivor) |>
        dplyr::arrange(domestic_violence_survivor)

    })

    # Create education pie chart
    output$victim_pie_chart <- echarts4r::renderEcharts4r({

      victim_pie_chart_data() |>
        pie_chart(
          category = "domestic_violence_survivor",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in line chart
    victim_sankey_chart_data <- shiny::reactive({

      validate_data(domestic_violence_data_filtered())

      ids_exited <- domestic_violence_data_filtered() |>
        dplyr::filter(
          domestic_violence_survivor %in% c("Yes", "No")
        ) |>
        get_ids_for_sankey()

      validate_data(ids_exited)

      domestic_violence_data_filtered() |>
        dplyr::filter(
          domestic_violence_survivor %in% c("Yes", "No")
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = domestic_violence_survivor)

    })

    # Create disabilities trend line chart
    output$victim_sankey_chart <- echarts4r::renderEcharts4r({

      victim_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
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

    # Create reactive data frame to data to be displayed in pie chart
    when_occurred_pie_chart_data <- shiny::reactive({

      out <- domestic_violence_data_filtered() |>
        dplyr::filter(
          !when_occurred %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(when_occurred)
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          when_occurred,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          when_occurred
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          when_occurred,
          .keep_all = TRUE
        )

      validate_data(out)

      out |>
        dplyr::count(when_occurred) |>
        dplyr::arrange(when_occurred)

    })

    # Create education pie chart
    output$when_occurred_pie_chart <- echarts4r::renderEcharts4r({

      when_occurred_pie_chart_data() |>
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

    # Create reactive data frame to data to be displayed in pie chart
    currently_fleeing_pie_chart_data <- shiny::reactive({

      out <- domestic_violence_data_filtered() |>
        dplyr::filter(
          !currently_fleeing %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(currently_fleeing)
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          currently_fleeing,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          currently_fleeing
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          currently_fleeing,
          .keep_all = TRUE
        )

      validate_data(out)

      out |>
        dplyr::count(currently_fleeing) |>
        dplyr::arrange(currently_fleeing)

    })

    # Create education pie chart
    output$currently_fleeing_pie_chart <- echarts4r::renderEcharts4r({

      currently_fleeing_pie_chart_data() |>
        pie_chart(
          category = "currently_fleeing",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in line chart
    currently_fleeing_sankey_chart_data <- shiny::reactive({

      ids_exited <- domestic_violence_data_filtered() |>
        dplyr::filter(
          !currently_fleeing %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(currently_fleeing)
        ) |>
        get_ids_for_sankey()

      validate_data(ids_exited)

      domestic_violence_data_filtered() |>
        dplyr::filter(
          !currently_fleeing %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(currently_fleeing)
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = currently_fleeing)

    })

    # Create disabilities trend line chart
    output$currently_fleeing_sankey_chart <- echarts4r::renderEcharts4r({

      currently_fleeing_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
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
