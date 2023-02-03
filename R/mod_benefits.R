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

    # Info Boxes ----
    shiny::fluidRow(

      shiny::column(
        width = 6,
        ## Number of youth (post filters) ----
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_box"),
          width = "100%"
        )
      ),

      shiny::column(
        width = 6,
        ## Number of youth with benefits data (post filters) ----
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_benefits_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    # Visuals ----
    shiny::fluidRow(

      shiny::column(
        width = 12,

        bs4Dash::tabsetPanel(
          type = "pills",

          ## Benefits Tab Panel ----
          shiny::tabPanel(
            title = "Benefits",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                ### Benefits Pie Chart ----
                bs4Dash::box(
                  title = "Benefits Received (from Any Source)",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("benefits_pie_chart"),
                    height = "400px"
                  )
                )

              ),

              shiny::column(
                width = 6,

                ### Benefits Source Pie Chart ----
                bs4Dash::box(
                  title = "Benefits Received by Source",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("benefits_source_pie_chart"),
                    height = "400px"
                  )
                )

              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                ### Benefits Sankey Chart ----
                bs4Dash::box(
                  title = "Changes in Benefits (from Any Source) Response (Entry --> Exit)",
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

                ### Benefits Data Quality Table ----
                bs4Dash::box(
                  title = "Benefits Data Quality Statistics",
                  width = NULL,
                  maximizable = TRUE,
                  reactable::reactableOutput(
                    outputId = ns("benefits_missingness_stats_tbl")
                  )
                )

              )
            )

          ),

          ## Health Insurance Tab Panel ----
          shiny::tabPanel(
            title = "Health Insurance",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                ### Health Insurance Pie Chart ----
                bs4Dash::box(
                  title = "Health Insurance Received (from Any Source)",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("insurance_pie_chart"),
                    height = "400px"
                  )
                )

              ),

              shiny::column(
                width = 6,

                ### Health Insurance Source Pie Chart ----
                bs4Dash::box(
                  title = "Health Insurance Received by Source",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("insurance_source_pie_chart"),
                    height = "400px"
                  )
                )

              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                ### Health Insurance Sankey Chart ----
                bs4Dash::box(
                  title = "Changes in Health Insurance (from Any Source) Response (Entry --> Exit)",
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

                ### Health Insurance Data Quality Table ----
                bs4Dash::box(
                  title = "Health Insurance Data Quality Statistics",
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

    # Info Boxes ----

    ## Total number of youth (post filters) ----
    n_youth <- shiny::reactive(

      clients_filtered() |>
        nrow()

    )

    ## Apply the filters to the benefits data ----
    benefits_data_filtered <- shiny::reactive({

      benefits_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    })

    ## Filtered number of youth with benefits data ----
    n_youth_with_benefits_data <- shiny::reactive(

      benefits_data_filtered() |>
        dplyr::filter(
          benefits_from_any_source %in% c("Yes", "No")
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    ## Render "number of clients" info box ----
    output$n_youth_box <- bs4Dash::renderbs4ValueBox(

      bs4Dash::bs4ValueBox(
        value = n_youth(),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user")
      )

    )

    ## Render "number of youth with benefits data" info box ----
    output$n_youth_with_benefits_data_box <- bs4Dash::renderbs4ValueBox(

      bs4Dash::bs4ValueBox(
        value = n_youth_with_benefits_data(),
        subtitle = "Total # of Youth with Benefits Data Available",
        icon = shiny::icon("home")
      )

    )

    ## Benefits Pie Chart ----

    ### Get data for benefits pie chart ----
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

    ### Render benefits pie chart ----
    output$benefits_pie_chart <- echarts4r::renderEcharts4r(

      benefits_pie_chart_data() |>
        pie_chart(
          category = "benefits_from_any_source",
          count = "n"
        )

    )

    ## Health Insurance Pie Chart ----

    ### Get data for insurance pie chart ----
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
        dplyr::arrange(insurance_from_any_source) |>
        dplyr::rename(health_insurance_from_any_source = insurance_from_any_source)

    })

    ### Render insurance pie chart ----
    output$insurance_pie_chart <- echarts4r::renderEcharts4r({

      insurance_pie_chart_data() |>
        pie_chart(
          category = "health_insurance_from_any_source",
          count = "n"
        )

    })

    ## Benefits Source Pie Chart ----

    ### Get data for benefits source pie chart ----
    benefits_source_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(benefits_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- benefits_data_filtered() |>
        dplyr::select(
          organization_id,
          personal_id,
          snap:other_benefits_source,
          date_updated
        ) |>
        tidyr::pivot_longer(
          cols = snap:other_benefits_source,
          names_to = "benefits_source",
          values_to = "response"
        ) |>
        dplyr::filter(response == "Yes") |>
        dplyr::arrange(
          organization_id,
          personal_id,
          benefits_source,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          benefits_source
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(benefits_source) |>
        dplyr::mutate(
          benefits_source = janitor::make_clean_names(
            string = benefits_source,
            case = "title"
          )
        ) |>
        dplyr::arrange(benefits_source)

    })

    ### Render benefits source pie chart ----
    output$benefits_source_pie_chart <- echarts4r::renderEcharts4r({

      benefits_source_pie_chart_data() |>
        pie_chart(
          category = "benefits_source",
          count = "n"
        )

    })

    ## Health Insurance Source Pie Chart ----

    ### Get data for insurance source pie chart ----
    insurance_source_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(benefits_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- benefits_data_filtered() |>
        dplyr::select(
          organization_id,
          personal_id,
          medicaid,
          medicare,
          schip,
          va_medical_services,
          employer_provided,
          cobra,
          private_pay,
          state_health_ins,
          indian_health_services,
          other_insurance,
          date_updated
        ) |>
        tidyr::pivot_longer(
          cols = medicaid:other_insurance,
          names_to = "insurance_source",
          values_to = "response"
        ) |>
        dplyr::filter(response == "Yes") |>
        dplyr::arrange(
          organization_id,
          personal_id,
          insurance_source,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          insurance_source
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(insurance_source) |>
        dplyr::mutate(
          insurance_source = janitor::make_clean_names(
            string = insurance_source,
            case = "title"
          )
        ) |>
        dplyr::arrange(insurance_source) |>
        dplyr::rename(health_insurance_source = insurance_source)

    })

    ### Render insurance source pie chart ----
    output$insurance_source_pie_chart <- echarts4r::renderEcharts4r({

      insurance_source_pie_chart_data() |>
        pie_chart(
          category = "health_insurance_source",
          count = "n"
        )

    })

    ## Benefits Sankey Chart ----

    ### Get data for benefits sankey chart ----
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

    ### Render benefits sankey chart ----
    output$benefits_sankey_chart <- echarts4r::renderEcharts4r(

      benefits_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    )

    ## Health Insurance Sankey Chart ----

    ### Get data for insurance sankey chart ----
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

    ### Render insurance sankey chart ----
    output$insurance_sankey_chart <- echarts4r::renderEcharts4r(

      insurance_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    )

    ## Data Quality Statistics ----

    ### Get data for benefits data quality table ----
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

    ### Render benefits data quality table ----
    output$benefits_missingness_stats_tbl <- reactable::renderReactable(

      reactable::reactable(
        benefits_missingness_stats()
      )

    )

    ### Get data for insurance data quality table ----
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

    ### Render insurance data quality table ----
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
