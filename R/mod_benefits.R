#' benefits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_benefits_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidRow(

      shiny::column(
        width = 6,
        # Number of youth (post filters)
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_box"),
          width = "100%"
        )
      ),

      shiny::column(
        width = 6,
        # Number of youth with benefits data (post filters)
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_benefits_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 12,

        bs4Dash::tabsetPanel(
          type = "pills",

          shiny::tabPanel(
            title = "Benefits (from any source)",

            shiny::fluidRow(

              shiny::column(
                width = 4,

                bs4Dash::box(
                  title = "# of Youth by Benefits Response",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("benefits_pie_chart"),
                    height = "400px"
                  )
                )

              ),

              shiny::column(
                width = 8,

                bs4Dash::box(
                  title = "Changes in Benefits Response (Entry --> Exit)",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("benefits_sankey_chart"),
                    height = "400px"
                  )
                )

              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = "Data Quality Statistics",
                  width = NULL,
                  maximizable = TRUE,
                  reactable::reactableOutput(
                    outputId = ns("benefits_missingness_stats_tbl")
                  )
                )

              )
            )

          ),

          shiny::tabPanel(
            title = "Insurance (from any source)",

            shiny::fluidRow(

              shiny::column(
                width = 4,

                bs4Dash::box(
                  title = "# of Youth by Insurance Response",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("insurance_pie_chart"),
                    height = "400px"
                  )
                )

              ),

              shiny::column(
                width = 8,

                bs4Dash::box(
                  title = "Changes in Insurance Response (Entry --> Exit)",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("insurance_sankey_chart"),
                    height = "400px"
                  )
                )

              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = "Data Quality Statistics",
                  width = NULL,
                  maximizable = TRUE,
                  reactable::reactableOutput(
                    outputId = ns("insurance_missingness_stats_tbl")
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

#' benefits Server Functions
#'
#' @noRd
mod_benefits_server <- function(id, benefits_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive(

      clients_filtered() |>
        nrow()

    )

    # Apply the filters to the benefits data
    benefits_data_filtered <- shiny::reactive({

      benefits_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    })

    # Total number of Youth in program(s) that exist in the `benefits.csv`
    # file
    n_youth_with_benefits_data <- shiny::reactive(

      benefits_data_filtered() |>
        dplyr::filter(
          benefits_from_any_source %in% c("Yes", "No")
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Render number of clients box
    output$n_youth_box <- bs4Dash::renderbs4ValueBox(

      bs4Dash::bs4ValueBox(
        value = n_youth(),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user")
      )

    )

    # Render number of projects box
    output$n_youth_with_benefits_data_box <- bs4Dash::renderbs4ValueBox(

      bs4Dash::bs4ValueBox(
        value = n_youth_with_benefits_data(),
        subtitle = "Total # of Youth with Benefits Data Available",
        icon = shiny::icon("home")
      )

    )

    # Create reactive data frame to data to be displayed in pie chart
    benefits_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(benefits_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- benefits_data_filtered() |>
        dplyr::filter(
          benefits_from_any_source %in% c("Yes", "No")
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          benefits_from_any_source,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          benefits_from_any_source
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          benefits_from_any_source,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(benefits_from_any_source) |>
        dplyr::arrange(benefits_from_any_source)

    })

    # Create benefits pie chart
    output$benefits_pie_chart <- echarts4r::renderEcharts4r(

      benefits_pie_chart_data() |>
        pie_chart(
          category = "benefits_from_any_source",
          count = "n"
        )

    )

    # Create reactive data frame to data to be displayed in pie chart
    insurance_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(benefits_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- benefits_data_filtered() |>
        dplyr::filter(
          insurance_from_any_source %in% c("Yes", "No")
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          insurance_from_any_source,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          insurance_from_any_source
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          insurance_from_any_source,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(insurance_from_any_source) |>
        dplyr::arrange(insurance_from_any_source)

    })

    # Create benefits pie chart
    output$insurance_pie_chart <- echarts4r::renderEcharts4r({

      insurance_pie_chart_data() |>
        pie_chart(
          category = "insurance_from_any_source",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in line chart
    benefits_sankey_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(benefits_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      ids_exited <- benefits_data_filtered() |>
        dplyr::filter(
          benefits_from_any_source %in% c("Yes", "No")
        ) |>
        get_ids_for_sankey()

      shiny::validate(
        shiny::need(
          expr = nrow(ids_exited) >= 1L,
          message = "No data to display"
        )
      )

      benefits_data_filtered() |>
        dplyr::filter(
          benefits_from_any_source %in% c("Yes", "No")
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = benefits_from_any_source)

    })

    # Create benefits sankey chart
    output$benefits_sankey_chart <- echarts4r::renderEcharts4r(

      benefits_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    )

    # Create reactive data frame to data to be displayed in line chart
    insurance_sankey_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(benefits_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      ids_exited <- benefits_data_filtered() |>
        dplyr::filter(
          insurance_from_any_source %in% c("Yes", "No")
        ) |>
        get_ids_for_sankey()

      shiny::validate(
        shiny::need(
          expr = nrow(ids_exited) >= 1L,
          message = "No data to display"
        )
      )

      benefits_data_filtered() |>
        dplyr::filter(
          insurance_from_any_source %in% c("Yes", "No")
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = insurance_from_any_source)

    })

    # Create insurance sankey chart
    output$insurance_sankey_chart <- echarts4r::renderEcharts4r(

      insurance_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    )

    benefits_missingness_stats <- shiny::reactive(

      benefits_data_filtered() |>
        dplyr::mutate(benefits_from_any_source = ifelse(
          is.na(benefits_from_any_source),
          "(Blank)",
          benefits_from_any_source
        )) |>
        dplyr::filter(
          !benefits_from_any_source %in% c("Yes", "No")
        ) |>
        dplyr::count(benefits_from_any_source, name = "Count") |>
        dplyr::rename(Response = benefits_from_any_source)

    )

    output$benefits_missingness_stats_tbl <- reactable::renderReactable(

      reactable::reactable(
        benefits_missingness_stats()
      )

    )

    insurance_missingness_stats <- shiny::reactive({

      benefits_data_filtered() |>
        dplyr::mutate(insurance_from_any_source = ifelse(
          is.na(insurance_from_any_source),
          "(Blank)",
          insurance_from_any_source
        )) |>
        dplyr::filter(
          !insurance_from_any_source %in% c("Yes", "No")
        ) |>
        dplyr::count(insurance_from_any_source, name = "Count") |>
        dplyr::rename(Response = insurance_from_any_source)

    })

    output$insurance_missingness_stats_tbl <- reactable::renderReactable(

      reactable::reactable(
        insurance_missingness_stats()
      )

    )

  })
}

## To be copied in the UI
# mod_benefits_ui("benefits_1")

## To be copied in the server
# mod_benefits_server("benefits_1")
