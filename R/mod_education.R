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

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth")),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user", class = "fa-solid"),
        width = 6
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_education")),
        subtitle = "Total # of Youth with Education Data Available",
        icon = shiny::icon("book-open"),
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
            title = "Last Grade Completed",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Youth by Last Grade Completed Group",
                    content = shiny::tagList(
                      shiny::span("Response categories have been grouped to improve chart readability."),
                      shiny::br(),
                      link_section("R4 Last Grade Completed")
                    )
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("last_grade_completed_pie_chart"),
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
                    outputId = ns("last_grade_completed_missingness_stats_tbl")
                  )
                )

              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "Changes in Last Grade Completed Group (Entry --> Exit)",
                    content = shiny::tagList(
                      shiny::span("Response categories have been grouped to improve chart readability."),
                      shiny::br(),
                      link_section("R4 Last Grade Completed")
                    )
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("last_grade_completed_sankey_chart"),
                    height = "100%"
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
                  title = with_popover(
                    text = "# of Youth by School Status",
                    content = link_section("R5 School Status")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("school_status_pie_chart"),
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
                    outputId = ns("school_status_missingness_stats_tbl")
                  )
                )

              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "Changes in School Status (Entry --> Exit)",
                    content = link_section("R5 School Status")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("school_status_sankey_chart"),
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
          by = c("personal_id", "organization_id", "enrollment_id")
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
          ),
          last_grade_completed = factor(
            last_grade_completed,
            levels = c(
              "Less than Grade 5",
              "Grades 5-8",
              "Grades 9-11",
              "High school diploma/GED",
              "Some College",
              "College Degree/Vocational",
              "Unknown"
            ),
            ordered = TRUE
          )
        )

    })

    # Total number of Youth in program(s) that exist in the `education.csv`
    # file
    n_youth_with_education_data <- shiny::reactive(

      education_data_filtered() |>
        dplyr::filter(
          last_grade_completed != "Unknown",
          !is.na(last_grade_completed)
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    # Render number of projects box value
    output$n_youth_with_education <- shiny::renderText({
      n_youth_with_education_data()
    })

    # Last Grade Completed ----

    ## Pie Chart ----

    # Create reactive data frame to data to be displayed in pie chart
    last_grade_completed_pie_chart_data <- shiny::reactive({
      validate_data(education_data_filtered())

      out <- education_data_filtered() |>
        dplyr::filter(
          last_grade_completed != "Unknown",
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

      validate_data(out)

      out |>
        dplyr::count(last_grade_completed) |>
        dplyr::arrange(last_grade_completed) |>
        dplyr::mutate(last_grade_completed = as.character(last_grade_completed))

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


      ids_exited <- education_data_filtered() |>
        dplyr::filter(
          last_grade_completed != "Unknown",
          !is.na(last_grade_completed)
        ) |>
        get_ids_for_sankey()

      shiny::validate(
        shiny::need(
          expr = nrow(ids_exited) >= 1L,
          message = "No data to display"
        )
      )
      validate_data(education_data_filtered())

      education_data_filtered() |>
        dplyr::filter(
          last_grade_completed != "Unknown",
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
                "Less than Grade 5",
                "Grades 5-8",
                "Grades 9-11",
                "Some College",
                "High school diploma/GED",
                "College Degree/Vocational"
              ),
              " (Entry)"
            ),
            ordered = TRUE
          ),
          Exit = factor(
            Exit,
            levels = paste0(
              c(
                "Less than Grade 5",
                "Grades 5-8",
                "Grades 9-11",
                "Some College",
                "High school diploma/GED",
                "College Degree/Vocational"
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
      validate_data(last_grade_completed_sankey_chart_data())

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

      validate_data(education_data_filtered())

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

      validate_data(out)

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

      validate_data(education_data_filtered())

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

      validate_data(ids_exited)

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
