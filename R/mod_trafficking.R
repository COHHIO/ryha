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

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth")),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user", class = "fa-solid"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_sex_data")),
        subtitle = "Total # of Youth with Sex Trafficking Data Available",
        icon = shiny::icon("exclamation-circle"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_labor_data")),
        subtitle = "Total # of Youth with Labor Trafficking Data Available",
        icon = shiny::icon("exclamation-circle"),
        width = 4
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
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Head of Household and/or Adults by Exchange for Sex Response",
                    content = link_section("R15 Commercial Sexual Exploitation/Sex Trafficking")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("exchange_sex_chart"),
                    height = "100%"
                  )
                )

              ),

            ),

            shiny::fluidRow(

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Head of Household and/or Adults by Asked or Forced to Exchange Response",
                    content = shiny::tagList(
                      shiny::p("Only youth that ever received anything in exchange for sex are included."),
                      shiny::p(link_section("R15 Commercial Sexual Exploitation/Sex Trafficking"))
                    )
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("asked_sex_chart"),
                    height = "100%"
                  )
                )

              ),

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Head of Household and/or Adults by Count of Exchange for Sex Response",
                    content = shiny::tagList(
                      shiny::p("Only youth that ever received anything in exchange for sex are included."),
                      shiny::p(link_section("R15 Commercial Sexual Exploitation/Sex Trafficking"))
                    )
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("count_sex_chart"),
                    height = "100%"
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
                  title = with_popover(
                    text = "# of Head of Household and/or Adults by Workplace Violence/Threats Response",
                    content = link_section("R16 Labor Exploitation/Trafficking")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("violence_labor_chart"),
                    height = "100%"
                  )
                )

              ),

              shiny::column(
                width = 6,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Head of Household and/or Adults by Workplace Promise Difference Response",
                    content = link_section("R16 Labor Exploitation/Trafficking")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("promise_labor_chart"),
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
                    text = "# of Head of Household and/or Adults by Coerced to Continue Work Response",
                    content = shiny::tagList(
                      shiny::p("Only Head of Household and Adults that experienced workplace violence and/or promise difference are included."),
                      shiny::p(link_section("R16 Labor Exploitation/Trafficking"))
                    )
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("coerced_labor_chart"),
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

#' trafficking Server Functions
#'
#' @noRd
mod_trafficking_server <- function(id, trafficking_data, clients_filtered, heads_of_household_and_adults){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive(

      clients_filtered() |>
        nrow()

    )

    # Filter trafficking data
    trafficking_data_filtered <- shiny::reactive(
      filter_data(trafficking_data, clients_filtered()) |>
        dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
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

    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    # Render number of projects box value
    output$n_youth_with_sex_data <- shiny::renderText({
      n_youth_with_sex_data()
    })

    # Render number of projects box value
    output$n_youth_with_labor_data <- shiny::renderText({
      n_youth_with_labor_data()
    })

    # Sex Trafficking ----

    ## Exchange for Sex ----
    output$exchange_sex_chart <- echarts4r::renderEcharts4r({
      trafficking_data_filtered() |> 
        dplyr::count(exchange_for_sex, .drop = FALSE) |> 
        bar_chart(
          x = "exchange_for_sex",
          y = "n"
        )
    })

    ## Count of Exchange for Sex ----
    output$count_sex_chart <- echarts4r::renderEcharts4r({
      trafficking_data_filtered() |>
        dplyr::filter(exchange_for_sex == "Yes") |> 
        dplyr::count(count_of_exchange_for_sex, .drop = FALSE) |>
        bar_chart(
          x = "count_of_exchange_for_sex",
          y = "n"
        )
    })

    ## Asked or Forced to Exchange for Sex ----
    output$asked_sex_chart <- echarts4r::renderEcharts4r(
      trafficking_data_filtered() |>
        dplyr::filter(exchange_for_sex == "Yes") |> 
        dplyr::count(asked_or_forced_to_exchange_for_sex, .drop = FALSE) |>
        bar_chart(
          x = "asked_or_forced_to_exchange_for_sex",
          y = "n"
        )
    )

    # Labor Trafficking ----

    ## Workplace Violence Threats ----
    output$violence_labor_chart <- echarts4r::renderEcharts4r(
      trafficking_data_filtered() |>
        dplyr::count(work_place_violence_threats, .drop = FALSE) |>
        bar_chart(
          x = "work_place_violence_threats",
          y = "n"
        )
    )

    ## Workplace Promise Difference ----
    output$promise_labor_chart <- echarts4r::renderEcharts4r(
      trafficking_data_filtered() |>
        dplyr::count(workplace_promise_difference, .drop = FALSE) |>
        bar_chart(
          x = "workplace_promise_difference",
          y = "n"
        )
    )

    ## Coerced to Continue Work ----
    output$coerced_labor_chart <- echarts4r::renderEcharts4r({
      trafficking_data_filtered() |>
        dplyr::filter(work_place_violence_threats == "Yes" | workplace_promise_difference == "Yes") |>
        dplyr::count(coerced_to_continue_work, .drop = FALSE) |>
        bar_chart(
          x = "coerced_to_continue_work",
          y = "n"
        )
    })

  })
}

## To be copied in the UI
# mod_trafficking_ui("trafficking_1")

## To be copied in the server
# mod_trafficking_server("trafficking_1")
