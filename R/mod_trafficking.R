#' trafficking UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_trafficking_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidRow(

      shiny::column(
        width = 4,
        # Number of clients (post filters)
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_box"),
          width = "100%"
        )
      ),

      shiny::column(
        width = 4,
        # Number of youth with sex trafficking data
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_sex_data_box"),
          width = "100%"
        )
      ),

      shiny::column(
        width = 4,
        # Number of youth with labor trafficking data
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_labor_data_box"),
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
            title = "Sex Trafficking",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = "# of Youth by Exchange for Sex Response",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("exchange_sex_pie_chart"),
                    height = "300px"
                  )
                )

              ),

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = "# of Youth by Count of Exchange for Sex Response",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("count_sex_pie_chart"),
                    height = "300px"
                  )
                )

              )

            ),

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = "# of Youth by Asked or Forced to Exchange Response",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("asked_sex_pie_chart"),
                    height = "300px"
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
                    outputId = ns("sex_missingness_stats_tbl")
                  )
                )

              )

            )

          ),

          shiny::tabPanel(
            title = "Labor Trafficking",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = "# of Youth by Workplace Violence/Threats Response",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("violence_labor_pie_chart"),
                    height = "300px"
                  )
                )

              ),

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = "# of Youth by Workplace Promise Difference Response",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("promise_labor_pie_chart"),
                    height = "300px"
                  )
                )

              )

            ),

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = "# of Youth by Coerced to Continue Work Response",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("coerced_labor_pie_chart"),
                    height = "300px"
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
                    outputId = ns("labor_missingness_stats_tbl")
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

#' trafficking Server Functions
#'
#' @noRd
mod_trafficking_server <- function(id, trafficking_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive(

      clients_filtered() |>
        nrow()

    )

    # Apply the filters to the trafficking data
    trafficking_data_filtered <- shiny::reactive(

      trafficking_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    )

    # Total number of Youth in program(s) that exist in the `education.csv`
    # file
    n_youth_with_sex_data <- shiny::reactive(

      trafficking_data_filtered() |>
        dplyr::filter(
          exchange_for_sex %in% c("Yes", "No")
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Total number of Youth in program(s) that exist in the `education.csv`
    # file
    n_youth_with_labor_data <- shiny::reactive(

      trafficking_data_filtered() |>
        dplyr::filter(
          work_place_violence_threats %in% c("Yes", "No") |
            workplace_promise_difference %in% c("Yes", "No") |
            coerced_to_continue_work %in% c("Yes", "No")
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
    output$n_youth_with_sex_data_box <- bs4Dash::renderbs4ValueBox(

      bs4Dash::bs4ValueBox(
        value = n_youth_with_sex_data(),
        subtitle = "Total # of Youth with Sex Trafficking Data Available",
        icon = shiny::icon("home")
      )

    )

    # Render number of projects box
    output$n_youth_with_labor_data_box <- bs4Dash::renderbs4ValueBox(

      bs4Dash::bs4ValueBox(
        value = n_youth_with_labor_data(),
        subtitle = "Total # of Youth with Labor Trafficking Data Available",
        icon = shiny::icon("home")
      )

    )

    # Sex Trafficking ----

    ## Exchange for Sex ----

    # Create reactive data frame to data to be displayed in pie chart
    exchange_sex_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(trafficking_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- trafficking_data_filtered() |>
        dplyr::filter(
          exchange_for_sex %in% c("Yes", "No")
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          exchange_for_sex,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          exchange_for_sex
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          exchange_for_sex,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(exchange_for_sex) |>
        dplyr::arrange(exchange_for_sex)

    })

    # Create education pie chart
    output$exchange_sex_pie_chart <- echarts4r::renderEcharts4r(

      exchange_sex_pie_chart_data() |>
        pie_chart(
          category = "exchange_for_sex",
          count = "n"
        )

    )

    # Create missingness stats data frame
    exchange_sex_missingness_stats <- shiny::reactive({

      trafficking_data_filtered() |>
        dplyr::mutate(exchange_for_sex = ifelse(
          is.na(exchange_for_sex),
          "(Blank)",
          exchange_for_sex
        )) |>
        dplyr::filter(
          !exchange_for_sex %in% c("Yes", "No")
        ) |>
        dplyr::count(exchange_for_sex, name = "Count") |>
        dplyr::rename(Response = exchange_for_sex)

    })

    # Create missingness stats table for "exchange for sex"
    output$sex_missingness_stats_tbl <- reactable::renderReactable(

      reactable::reactable(
        exchange_sex_missingness_stats()
      )

    )

    ## Count of Exchange for Sex ----

    # Create reactive data frame to data to be displayed in pie chart
    count_sex_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(trafficking_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- trafficking_data_filtered() |>
        dplyr::filter(
          !count_of_exchange_for_sex %in% c(
            "Client doesn’t know",
            "Client prefers not to answer",
            "Data not collected"
          ),
          !is.na(count_of_exchange_for_sex)
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          count_of_exchange_for_sex,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          count_of_exchange_for_sex
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          count_of_exchange_for_sex,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(count_of_exchange_for_sex) |>
        dplyr::arrange(count_of_exchange_for_sex)

    })

    # Create pie chart
    output$count_sex_pie_chart <- echarts4r::renderEcharts4r(

      count_sex_pie_chart_data() |>
        pie_chart(
          category = "count_of_exchange_for_sex",
          count = "n"
        )

    )

    ## Asked or Forced to Exchange for Sex ----

    # Create reactive data frame to data to be displayed in pie chart
    asked_sex_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(trafficking_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- trafficking_data_filtered() |>
        dplyr::filter(
          asked_or_forced_to_exchange_for_sex %in% c("Yes", "No")
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          asked_or_forced_to_exchange_for_sex,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          asked_or_forced_to_exchange_for_sex
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          asked_or_forced_to_exchange_for_sex,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(asked_or_forced_to_exchange_for_sex) |>
        dplyr::arrange(asked_or_forced_to_exchange_for_sex)

    })

    # Create education pie chart
    output$asked_sex_pie_chart <- echarts4r::renderEcharts4r(

      asked_sex_pie_chart_data() |>
        pie_chart(
          category = "asked_or_forced_to_exchange_for_sex",
          count = "n"
        )

    )

    # Labor Trafficking ----

    ## Workplace Violence Threats ----

    # Create reactive data frame to data to be displayed in pie chart
    violence_labor_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(trafficking_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- trafficking_data_filtered() |>
        dplyr::filter(
          work_place_violence_threats %in% c("Yes", "No")
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          work_place_violence_threats,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          work_place_violence_threats
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          work_place_violence_threats,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(work_place_violence_threats) |>
        dplyr::arrange(work_place_violence_threats)

    })

    # Create workplace violence threats pie chart
    output$violence_labor_pie_chart <- echarts4r::renderEcharts4r(

      violence_labor_pie_chart_data() |>
        pie_chart(
          category = "work_place_violence_threats",
          count = "n"
        )

    )

    # Create missingness stats data frame
    labor_missingness_stats <- shiny::reactive({

      trafficking_data_filtered() |>
        dplyr::mutate(work_place_violence_threats = ifelse(
          is.na(work_place_violence_threats),
          "(Blank)",
          work_place_violence_threats
        )) |>
        dplyr::filter(
          !work_place_violence_threats %in% c("Yes", "No")
        ) |>
        dplyr::count(work_place_violence_threats, name = "Count") |>
        dplyr::rename(Response = work_place_violence_threats)

    })

    # Create missingness stats table for "exchange for sex"
    output$labor_missingness_stats_tbl <- reactable::renderReactable(

      reactable::reactable(
        labor_missingness_stats()
      )

    )

    ## Workplace Promise Difference ----

    # Create reactive data frame to data to be displayed in pie chart
    promise_labor_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(trafficking_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- trafficking_data_filtered() |>
        dplyr::filter(
          workplace_promise_difference %in% c("Yes", "No")
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          workplace_promise_difference,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          workplace_promise_difference
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          workplace_promise_difference,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(workplace_promise_difference) |>
        dplyr::arrange(workplace_promise_difference)

    })

    # Create workplace violence threats pie chart
    output$promise_labor_pie_chart <- echarts4r::renderEcharts4r(

      promise_labor_pie_chart_data() |>
        pie_chart(
          category = "workplace_promise_difference",
          count = "n"
        )

    )

    ## Coerced to Continue Work ----

    # Create reactive data frame to data to be displayed in pie chart
    coerced_labor_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(trafficking_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- trafficking_data_filtered() |>
        dplyr::filter(
          coerced_to_continue_work %in% c("Yes", "No")
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          coerced_to_continue_work,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          coerced_to_continue_work
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          coerced_to_continue_work,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(coerced_to_continue_work) |>
        dplyr::arrange(coerced_to_continue_work)

    })

    # Create workplace violence threats pie chart
    output$coerced_labor_pie_chart <- echarts4r::renderEcharts4r(

      coerced_labor_pie_chart_data() |>
        pie_chart(
          category = "coerced_to_continue_work",
          count = "n"
        )

    )

  })
}

## To be copied in the UI
# mod_trafficking_ui("trafficking_1")

## To be copied in the server
# mod_trafficking_server("trafficking_1")
