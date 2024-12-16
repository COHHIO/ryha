#' health UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_health_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidRow(

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth")),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user", class = "fa-solid"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_health")),
        subtitle = "Total # of Youth with Health Data Available",
        icon = shiny::icon("stethoscope"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_counseling")),
        subtitle = "Total # of Youth with Counseling Data Available",
        icon = shiny::icon("stethoscope"),
        width = 4
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 12,

        bs4Dash::tabsetPanel(
          type = "pills",

          # General Health ----

          shiny::tabPanel(
            title = "General",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Youth by General Health Status",
                    content = link_section("R7 General Health Status")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("general_pie_chart"),
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
                    outputId = ns("general_missingness_stats_tbl")
                  )
                )

              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "Changes in General Health Status (Entry --> Exit)",
                    content = link_section("R7 General Health Status")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("general_sankey_chart"),
                    height = "100%"
                  )
                )

              )
            )

          ),

          # Dental Health ----

          shiny::tabPanel(
            title = "Dental",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Youth by Dental Health Status",
                    content = link_section("R8 Dental Health Status")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("dental_pie_chart"),
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
                    outputId = ns("dental_missingness_stats_tbl")
                  )
                )
              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "Changes in Dental Health Status (Entry --> Exit)",
                    content = link_section("R8 Dental Health Status")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("dental_sankey_chart"),
                    height = "100%"
                  )
                )

              )
            )

          ),

          # Mental Health ----

          shiny::tabPanel(
            title = "Mental",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Youth by Mental Health Status",
                    content = link_section("R9 Mental Health Status")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("mental_pie_chart"),
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
                    outputId = ns("mental_missingness_stats_tbl")
                  )
                )

              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "Changes in Mental Health Status (Entry --> Exit)",
                    content = link_section("R9 Mental Health Status")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("mental_sankey_chart"),
                    height = "100%"
                  )
                )

              )
            )

          ),

          # Counseling ----

          shiny::tabPanel(
            title = "Counseling",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Youth by Counseling Received Response",
                    content = link_section("R18 Counseling")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("counseling_pie_chart"),
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
                    outputId = ns("counseling_missingness_stats_tbl")
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

#' health Server Functions
#'
#' @noRd
mod_health_server <- function(id, health_data, counseling_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the health data
    health_data_filtered <- shiny::reactive({

      health_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id", "enrollment_id")
        )

    })

    # Apply the filters to the counseling data
    counseling_data_filtered <- shiny::reactive({

      counseling_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id", "enrollment_id")
        )

    })

    # Total number of Youth in with health data available
    n_youth_with_health_data <- shiny::reactive(

      health_data_filtered() |>
        dplyr::filter(
          general_health_status %in% HealthStatusCodes$Description[1:5] |
            dental_health_status %in% HealthStatusCodes$Description[1:5] |
            mental_health_status %in% HealthStatusCodes$Description[1:5]
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Total number of Youth in with counseling data available
    n_youth_with_counseling_data <- shiny::reactive(

      counseling_data_filtered() |>
        dplyr::filter(
          counseling_received %in% c("Yes", "No")
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    # Render number of projects box
    output$n_youth_with_health <- shiny::renderText({
      n_youth_with_health_data()
    })

    # Render number of projects box
    output$n_youth_with_counseling <- shiny::renderText({
      n_youth_with_counseling_data()
    })

    # General Health ----

    # Create reactive data frame to data to be displayed in pie chart
    general_pie_chart_data <- shiny::reactive({

      validate_data(health_data_filtered())

      out <- health_data_filtered() |>
        dplyr::filter(
          !general_health_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(general_health_status)
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          general_health_status,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          general_health_status
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          general_health_status,
          .keep_all = TRUE
        )

      validate_data(out)

      out |>
        dplyr::count(general_health_status) |>
        dplyr::arrange(general_health_status)

    })

    # Create general health status pie chart
    output$general_pie_chart <- echarts4r::renderEcharts4r({

      general_pie_chart_data() |>
        pie_chart(
          category = "general_health_status",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in sankey chart
    general_sankey_chart_data <- shiny::reactive({

      validate_data(health_data_filtered())

      ids_exited <- health_data_filtered() |>
        dplyr::filter(
          !general_health_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(general_health_status)
        ) |>
        get_ids_for_sankey()

      validate_data(ids_exited)

      health_data_filtered() |>
        dplyr::filter(
          !general_health_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(general_health_status)
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = general_health_status) |>
        dplyr::mutate(
          Entry = factor(
            Entry,
            levels = paste0(HealthStatusCodes$Description[1:5], " (Entry)"),
            ordered = TRUE
          ),
          Exit = factor(
            Exit,
            levels = paste0(HealthStatusCodes$Description[1:5], " (Exit)"),
            ordered = TRUE
          )
        ) |>
        dplyr::arrange(Entry, Exit)

    })

    # Create general health status sankey chart
    output$general_sankey_chart <- echarts4r::renderEcharts4r({

      general_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    })

    # Capture the data quality statistics for "general_health_status" field
    general_missingness_stats <- shiny::reactive(

      health_data_filtered() |>
        dplyr::mutate(general_health_status = ifelse(
          is.na(general_health_status),
          "(Blank)",
          general_health_status
        )) |>
        dplyr::filter(
          general_health_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(general_health_status, name = "Count") |>
        dplyr::rename(Response = general_health_status)

    )

    # Create the {reactable} table to hold the missingness stats
    output$general_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        general_missingness_stats()
      )
    )

    # Dental Health ----

    # Create reactive data frame to data to be displayed in pie chart
    dental_pie_chart_data <- shiny::reactive({

      validate_data(health_data_filtered())

      out <- health_data_filtered() |>
        dplyr::filter(
          !dental_health_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(dental_health_status)
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          dental_health_status,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          dental_health_status
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          dental_health_status,
          .keep_all = TRUE
        )

      validate_data(out)

      out |>
        dplyr::count(dental_health_status) |>
        dplyr::arrange(dental_health_status)

    })

    # Create dental health status pie chart
    output$dental_pie_chart <- echarts4r::renderEcharts4r({

      dental_pie_chart_data() |>
        pie_chart(
          category = "dental_health_status",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in sankey chart
    dental_sankey_chart_data <- shiny::reactive({

      validate_data(health_data_filtered())

      ids_exited <- health_data_filtered() |>
        dplyr::filter(
          !dental_health_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(dental_health_status)
        ) |>
        get_ids_for_sankey()

      validate_data(ids_exited)

      health_data_filtered() |>
        dplyr::filter(
          !dental_health_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(dental_health_status)
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = dental_health_status) |>
        dplyr::mutate(
          Entry = factor(
            Entry,
            levels = paste0(HealthStatusCodes$Description[1:5], " (Entry)"),
            ordered = TRUE
          ),
          Exit = factor(
            Exit,
            levels = paste0(HealthStatusCodes$Description[1:5], " (Exit)"),
            ordered = TRUE
          )
        ) |>
        dplyr::arrange(Entry, Exit)

    })

    # Create dental health status sankey chart
    output$dental_sankey_chart <- echarts4r::renderEcharts4r({

      dental_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    })

    # Capture the data quality statistics for "dental_health_status" field
    dental_missingness_stats <- shiny::reactive({

      health_data_filtered() |>
        dplyr::mutate(dental_health_status = ifelse(
          is.na(dental_health_status),
          "(Blank)",
          dental_health_status
        )) |>
        dplyr::filter(
          dental_health_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(dental_health_status, name = "Count") |>
        dplyr::rename(Response = dental_health_status)

    })

    # Create the {reactable} table to hold the missingness stats
    output$dental_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        dental_missingness_stats()
      )
    )

    # Mental Health ----

    # Create reactive data frame to data to be displayed in pie chart
    mental_pie_chart_data <- shiny::reactive({

      validate_data(health_data_filtered())

      out <- health_data_filtered() |>
        dplyr::filter(
          !mental_health_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(mental_health_status)
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          mental_health_status,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          mental_health_status
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          mental_health_status,
          .keep_all = TRUE
        )

      validate_data(out)

      out |>
        dplyr::count(mental_health_status) |>
        dplyr::arrange(mental_health_status)

    })

    # Create mental health status pie chart
    output$mental_pie_chart <- echarts4r::renderEcharts4r({

      mental_pie_chart_data() |>
        pie_chart(
          category = "mental_health_status",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in sankey chart
    mental_sankey_chart_data <- shiny::reactive({

      validate_data(health_data_filtered())

      ids_exited <- health_data_filtered() |>
        dplyr::filter(
          !mental_health_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(mental_health_status)
        ) |>
        get_ids_for_sankey()

      validate_data(ids_exited)

      health_data_filtered() |>
        dplyr::filter(
          !mental_health_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(mental_health_status)
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = mental_health_status) |>
        dplyr::mutate(
          Entry = factor(
            Entry,
            levels = paste0(HealthStatusCodes$Description[1:5], " (Entry)"),
            ordered = TRUE
          ),
          Exit = factor(
            Exit,
            levels = paste0(HealthStatusCodes$Description[1:5], " (Exit)"),
            ordered = TRUE
          )
        ) |>
        dplyr::arrange(Entry, Exit)

    })

    # Create mental health status sankey chart
    output$mental_sankey_chart <- echarts4r::renderEcharts4r({

      mental_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    })

    # Capture the data quality statistics for "mental_health_status" field
    mental_missingness_stats <- shiny::reactive(

      health_data_filtered() |>
        dplyr::mutate(mental_health_status = ifelse(
          is.na(mental_health_status),
          "(Blank)",
          mental_health_status
        )) |>
        dplyr::filter(
          mental_health_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(mental_health_status, name = "Count") |>
        dplyr::rename(Response = mental_health_status)

    )

    # Create the {reactable} table to hold the missingness stats
    output$mental_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        mental_missingness_stats()
      )
    )

    # Counseling ----

    # Create reactive data frame to data to be displayed in pie chart
    counseling_pie_chart_data <- shiny::reactive({

      validate_data(counseling_data_filtered())

      # Keep the most recently updated data for each individual
      out <- counseling_data_filtered() |>
        dplyr::filter(
          counseling_received %in% c("Yes", "No")
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          counseling_received,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          counseling_received
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          counseling_received,
          .keep_all = TRUE
        )

      validate_data(out)

      out |>
        dplyr::count(counseling_received) |>
        dplyr::arrange(counseling_received)

    })

    # Create counseling pie chart
    output$counseling_pie_chart <- echarts4r::renderEcharts4r(

      counseling_pie_chart_data() |>
        pie_chart(
          category = "counseling_received",
          count = "n"
        )

    )

    # Capture the data quality statistics for "counseling_received" field
    counseling_missingness_stats <- shiny::reactive(

      counseling_data_filtered() |>
        dplyr::mutate(counseling_received = ifelse(
          is.na(counseling_received),
          "(Blank)",
          counseling_received
        )) |>
        dplyr::filter(
          !counseling_received %in% c("Yes", "No")
        ) |>
        dplyr::count(counseling_received, name = "Count") |>
        dplyr::rename(Response = counseling_received)

    )

    # Create the {reactable} table to hold the missingness stats
    output$counseling_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        counseling_missingness_stats()
      )
    )

  })
}

## To be copied in the UI
# mod_health_ui("health_1")
## To be copied in the server
# mod_health_server("health_1")
