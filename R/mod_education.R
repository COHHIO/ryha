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
                width = 6,

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
                width = 6,

                bs4Dash::box(
                  title = "Data Quality Statistics",
                  width = NULL,
                  maximizable = TRUE,
                  reactable::reactableOutput(
                    outputId = ns("last_grade_completed_missingness_stats_tbl")
                  )
                )

              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

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
            )

          ),

          shiny::tabPanel(
            title = "School Status",

            shiny::fluidRow(

              shiny::column(
                width = 6,

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
                width = 6,

                bs4Dash::box(
                  title = "Data Quality Statistics",
                  width = NULL,
                  maximizable = TRUE,
                  reactable::reactableOutput(
                    outputId = ns("school_status_missingness_stats_tbl")
                  )
                )

              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

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
        ) |>
        # Bucket Last Grade Completed categories
        dplyr::mutate(
          last_grade_completed = dplyr::case_when(
            last_grade_completed == "Less than Grade 5" ~ "Less than Grade 5",
            last_grade_completed == "Grades 5-6" ~ "Grades 5-8",
            last_grade_completed == "Grades 7-8" ~ "Grades 5-8",
            last_grade_completed == "Grades 9-11" ~ "Grades 9-11",
            last_grade_completed == "Grades 12 / High school diploma" ~ "High school diploma/GED",
            last_grade_completed == "School program does not have grade levels" ~ "Unknown",
            last_grade_completed == "GED" ~ "High school diploma/GED",
            last_grade_completed == "Some College" ~ "Some College",
            last_grade_completed == "Associate's Degree" ~ "College Degree/Vocational",
            last_grade_completed == "Bachelor's Degree" ~ "College Degree/Vocational",
            last_grade_completed == "Graduate Degree" ~ "College Degree/Vocational",
            last_grade_completed == "Vocational Degree" ~ "College Degree/Vocational",
            last_grade_completed == "Client doesn't know" ~ "Unknown",
            last_grade_completed == "Client refused" ~ "Unknown",
            last_grade_completed == "Data not collected" ~ "Unknown"
          )
        )

    })

    # Total number of Youth in program(s) that exist in the `education.csv`
    # file
    n_youth_with_education_data <- shiny::reactive(

      education_data_filtered() |>
        dplyr::filter(
          !last_grade_completed %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
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

    # Last Grade Completed ----

    ## Pie Chart ----

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
            "Client prefers not to answer",
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

    ## Sankey Chart ----

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
            "Client prefers not to answer",
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
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(last_grade_completed)
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = last_grade_completed) |>
        dplyr::mutate(
          Entry = factor(
            Entry,
            levels = paste0(
              c(
                LastGradeCompletedCodes$Description[6],
                LastGradeCompletedCodes$Description[1:5],
                LastGradeCompletedCodes$Description[7:9],
                LastGradeCompletedCodes$Description[12],
                LastGradeCompletedCodes$Description[10:11]
              ),
              " (Entry)"
            ),
            ordered = TRUE
          ),
          Exit = factor(
            Exit,
            levels = paste0(
              c(
                LastGradeCompletedCodes$Description[6],
                LastGradeCompletedCodes$Description[1:5],
                LastGradeCompletedCodes$Description[7:9],
                LastGradeCompletedCodes$Description[12],
                LastGradeCompletedCodes$Description[10:11]
              ),
              " (Exit)"
            ),
            ordered = TRUE
          )
        ) |>
        dplyr::arrange(Entry, Exit)

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

    # School Status ----

    ## Pie Chart ----

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
            "Client prefers not to answer",
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

    ## Sankey Chart ----

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
            "Client prefers not to answer",
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
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(school_status)
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = school_status) |>
        dplyr::mutate(
          Entry = factor(
            Entry,
            levels = paste0(
              c(
                SchoolStatusCodes$Description[4],
                SchoolStatusCodes$Description[3],
                SchoolStatusCodes$Description[1:2],
                SchoolStatusCodes$Description[6],
                SchoolStatusCodes$Description[7],
                SchoolStatusCodes$Description[5]
              ),
              " (Entry)"
            ),
            ordered = TRUE
          ),
          Exit = factor(
            Exit,
            levels = paste0(
              c(
                SchoolStatusCodes$Description[4],
                SchoolStatusCodes$Description[3],
                SchoolStatusCodes$Description[1:2],
                SchoolStatusCodes$Description[6],
                SchoolStatusCodes$Description[7],
                SchoolStatusCodes$Description[5]
              ),
              " (Exit)"
            ),
            ordered = TRUE
          )
        ) |>
        dplyr::arrange(Entry, Exit)

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
            "Client prefers not to answer",
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
            "Client prefers not to answer",
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
