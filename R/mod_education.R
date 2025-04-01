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
        width = 12,

        bs4Dash::tabsetPanel(
          type = "pills",

          shiny::tabPanel(
            title = "Last Grade Completed",

            shiny::fluidRow(

              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Head of Household and/or Adults by Last Grade Completed Group",
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
                    outputId = ns("last_grade_completed_chart"),
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
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Head of Household and/or Adults by School Status",
                    content = link_section("R5 School Status")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("school_status_chart"),
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
mod_education_server <- function(id, education_data, clients_filtered, heads_of_household_and_adults){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Filter education data
    education_data_filtered <- shiny::reactive({
      filter_data(education_data, clients_filtered()) |>
        dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
    })

    # Create reactive with the most recent data collected per enrollment
    most_recent_data_per_enrollment <- shiny::reactive({
      education_data_filtered() |>
        filter_most_recent_data_per_enrollment()
    })

    # Last Grade Completed ----
    output$last_grade_completed_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |>
        dplyr::count(last_grade_completed_grouped, .drop = FALSE) |>
        bar_chart(
          x = "last_grade_completed_grouped",
          y = "n"
        )
    })

    ## Sankey Chart ----
    output$last_grade_completed_sankey_chart <- echarts4r::renderEcharts4r({
      education_data_filtered() |>
        prepare_sankey_data(
          response_col = "last_grade_completed_grouped",
          response_vals = c(
            "Less than Grade 5",
            "Grades 5-8",
            "Grades 9-11",
            "Some College",
            "High school diploma/GED",
            "College Degree/Vocational"
          )
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

    # School Status ----
    output$school_status_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |>
        dplyr::count(school_status, .drop = FALSE) |> 
        bar_chart(
          x = "school_status",
          y = "n"
        )
    })

    ## Sankey Chart ----
    # Create "School Status" sankey chart
    output$school_status_sankey_chart <- echarts4r::renderEcharts4r({
      education_data_filtered() |>
        prepare_sankey_data(
          response_col = "school_status",
          response_vals = c(
            "Obtained GED",
            "Graduated from high school",
            "Attending school regularly",
            "Attending school irregularly",
            "Suspended",
            "Expelled"
          )
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

  })
}

## To be copied in the UI
# mod_education_ui("education_1")

## To be copied in the server
# mod_education_server("education_1")
