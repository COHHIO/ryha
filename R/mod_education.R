#' education UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_education_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidRow(

      shiny::column(
        width = 6,
        # Number of clients (post filters)
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_box"),
          width = "100%"
        )
      ),

      shiny::column(
        width = 6,
        # Number of projects (post filters)
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_education_data_box"),
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
            title = "Last Grade Completed",

            shiny::fluidRow(

              shiny::column(
                width = 4,

                bs4Dash::box(
                  title = "# of Youth by Last Grade Completed",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("last_grade_completed_pie_chart"),
                    height = "400px"
                  )
                )

              ),

              shiny::column(
                width = 8,

                bs4Dash::box(
                  title = "Changes in Last Grade Completed (Entry --> Exit)",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("last_grade_completed_sankey_chart"),
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
                    outputId = ns("last_grade_completed_missingness_stats_tbl")
                  )
                )

              )
            )

          ),

          shiny::tabPanel(
            title = "School Status",

            shiny::fluidRow(

              shiny::column(
                width = 4,

                bs4Dash::box(
                  title = "# of Youth by School Status",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("school_status_pie_chart"),
                    height = "400px"
                  )
                )

              ),

              shiny::column(
                width = 8,

                bs4Dash::box(
                  title = "Changes in School Status (Entry --> Exit)",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("school_status_sankey_chart"),
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
                    outputId = ns("school_status_missingness_stats_tbl")
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

#' education Server Functions
#'
#' @noRd
mod_education_server <- function(id, education_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the education data
    education_data_filtered <- shiny::reactive({

      education_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    })

    # Total number of Youth in program(s) that exist in the `education.csv`
    # file
    n_youth_with_education_data <- shiny::reactive(

      education_data_filtered() |>
        dplyr::filter(
          !last_grade_completed %in% c(
            "Client doesn't know",
            "Client refused",
            "Data not collected"
          ),
          !is.na(last_grade_completed)
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Render number of clients box
    output$n_youth_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth(),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user")
      )

    })

    # Render number of projects box
    output$n_youth_with_education_data_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_education_data(),
        subtitle = "Total # of Youth with Education Data Available",
        icon = shiny::icon("home")
      )

    })

    # Create reactive data frame to data to be displayed in pie chart
    last_grade_completed_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(education_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- education_data_filtered() |>
        dplyr::filter(
          !last_grade_completed %in% c(
            "Client doesn't know",
            "Client refused",
            "Data not collected"
          ),
          !is.na(last_grade_completed)
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          last_grade_completed,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          last_grade_completed
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          last_grade_completed,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(last_grade_completed) |>
        dplyr::arrange(last_grade_completed)

    })

    # Create education pie chart
    output$last_grade_completed_pie_chart <- echarts4r::renderEcharts4r({

      last_grade_completed_pie_chart_data() |>
        pie_chart(
          category = "last_grade_completed",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in pie chart
    school_status_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(education_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- education_data_filtered() |>
        dplyr::filter(
          !school_status %in% c(
            "Client doesn't know",
            "Client refused",
            "Data not collected"
          ),
          !is.na(school_status)
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          school_status,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          school_status
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          school_status,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(school_status) |>
        dplyr::arrange(school_status)

    })

    # Create education pie chart
    output$school_status_pie_chart <- echarts4r::renderEcharts4r({

      school_status_pie_chart_data() |>
        pie_chart(
          category = "school_status",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in line chart
    last_grade_completed_sankey_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(education_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      ids_exited <- education_data_filtered() |>
        dplyr::filter(
          !last_grade_completed %in% c(
            "Client doesn't know",
            "Client refused",
            "Data not collected"
          ),
          !is.na(last_grade_completed)
        ) |>
        get_ids_for_sankey()

      shiny::validate(
        shiny::need(
          expr = nrow(ids_exited) >= 1L,
          message = "No data to display"
        )
      )

      education_data_filtered() |>
        dplyr::filter(
          !last_grade_completed %in% c(
            "Client doesn't know",
            "Client refused",
            "Data not collected"
          ),
          !is.na(last_grade_completed)
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = last_grade_completed)

    })

    # Create disabilities trend line chart
    output$last_grade_completed_sankey_chart <- echarts4r::renderEcharts4r({

      last_grade_completed_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in line chart
    school_status_sankey_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(education_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      ids_exited <- education_data_filtered() |>
        dplyr::filter(
          !school_status %in% c(
            "Client doesn't know",
            "Client refused",
            "Data not collected"
          ),
          !is.na(school_status)
        ) |>
        get_ids_for_sankey()

      shiny::validate(
        shiny::need(
          expr = nrow(ids_exited) >= 1L,
          message = "No data to display"
        )
      )

      education_data_filtered() |>
        dplyr::filter(
          !school_status %in% c(
            "Client doesn't know",
            "Client refused",
            "Data not collected"
          ),
          !is.na(school_status)
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = school_status)

    })

    # Create "School Status" sankey chart
    output$school_status_sankey_chart <- echarts4r::renderEcharts4r({

      school_status_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    })

    last_grade_completed_missingness_stats <- shiny::reactive({

      education_data_filtered() |>
        dplyr::mutate(last_grade_completed = ifelse(
          is.na(last_grade_completed),
          "(Blank)",
          last_grade_completed
        )) |>
        dplyr::filter(
          last_grade_completed %in% c(
            "Client doesn't know",
            "Client refused",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(last_grade_completed, name = "Count") |>
        dplyr::rename(Response = last_grade_completed)

    })

    output$last_grade_completed_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        last_grade_completed_missingness_stats()
      )
    )

    school_status_missingness_stats <- shiny::reactive({

      education_data_filtered() |>
        dplyr::mutate(school_status = ifelse(
          is.na(school_status),
          "(Blank)",
          school_status
        )) |>
        dplyr::filter(
          school_status %in% c(
            "Client doesn't know",
            "Client refused",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(school_status, name = "Count") |>
        dplyr::rename(Response = school_status)

    })

    output$school_status_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        school_status_missingness_stats()
      )
    )

  })
}

## To be copied in the UI
# mod_education_ui("education_1")

## To be copied in the server
# mod_education_server("education_1")