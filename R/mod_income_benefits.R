#' income_benefits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_income_benefits_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Info Boxes ----
    shiny::fluidRow(

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth")),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user", class = "fa-solid"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_income_data")),
        subtitle = "Total # of Youth with Income Data Available",
        icon = shiny::icon("dollar-sign"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_benefits_data")),
        subtitle = "Total # of Youth with Benefits Data Available",
        icon = shiny::icon("dollar-sign"),
        width = 4
      )

    ),

    shiny::hr(),

    # Visuals ----
    shiny::fluidRow(

      shiny::column(
        width = 12,

        bs4Dash::tabsetPanel(
          type = "pills",

          ## Income Tab Panel ----
          shiny::tabPanel(
            title = "Income",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                ### Income Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Income Received (from Any Source)",
                    content = link_section("4.02 Income and Sources")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("income_chart"),
                    height = "100%"
                  )
                )

              ),

              shiny::column(
                width = 6,

                ### Income Source Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Income Received by Source (# of Youth with Income)",
                    content = shiny::tagList(
                      shiny::p("Only youth who reported receiving income are included."),
                      shiny::p("Each bar represents the percentage of youth who informed a given income source."),
                      shiny::p("Since individuals can select multiple sources, the total percentage may exceed 100%."),
                      shiny::p(link_section("4.02 Income and Sources"))
                    )
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("income_source_chart"),
                    height = "100%"
                  )
                )

              )

            ),

            shiny::fluidRow(

              shiny::column(
                width = 12,

                ### Monthly Income Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Total Monthly Income (# of Youth with Income)",
                    content = link_section("4.02 Income and Sources")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("income_bar_chart"),
                    height = "100%"
                  )
                )

              )

            )

          ),

          ## Benefits Tab Panel ----
          shiny::tabPanel(
            title = "Benefits",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                ### Benefits Pie Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Benefits Received (from Any Source)",
                    content = link_section("4.03 Non-Cash Benefits")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("benefits_chart"),
                    height = "100%"
                  )
                )

              ),

              shiny::column(
                width = 6,

                ### Benefits Source Pie Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Benefits Received by Source (# of Youth with Benefits)",
                    content = shiny::tagList(
                      shiny::p("Only youth who reported receiving benefits are included."),
                      shiny::p("Each bar represents the percentage of youth who informed a given benefit source."),
                      shiny::p("Since individuals can select multiple sources, the total percentage may exceed 100%."),
                      shiny::p(link_section("4.03 Non-Cash Benefits"))
                    )
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("benefits_source_chart"),
                    height = "100%"
                  )
                )

              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                ### Benefits Sankey Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Changes in Benefits (from Any Source) Response (Entry --> Exit)",
                    content = link_section("4.03 Non-Cash Benefits")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("benefits_sankey_chart"),
                    height = "100%"
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
                  title = with_popover(
                    text = "Health Insurance Received (from Any Source)",
                    content = link_section("4.04 Health Insurance")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("insurance_chart"),
                    height = "100%"
                  )
                )

              ),

              shiny::column(
                width = 6,

                ### Health Insurance Source Pie Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Health Insurance Received by Source (# of Youth with Insurance)",
                    content = shiny::tagList(
                      shiny::p("Only youth who reported receiving health insurance are included."),
                      shiny::p("Each bar represents the percentage of youth who informed a given health insurance source."),
                      shiny::p("Since individuals can select multiple sources, the total percentage may exceed 100%."),
                      shiny::p(link_section("4.04 Health Insurance"))
                    )
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("insurance_source_chart"),
                    height = "100%"
                  )
                )

              )

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                ### Health Insurance Sankey Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Changes in Health Insurance (from Any Source) Response (Entry --> Exit)",
                    content = link_section("4.04 Health Insurance")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("insurance_sankey_chart"),
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

#' income_benefits Server Functions
#'
#' @noRd
mod_income_benefits_server <- function(id, income_data, benefits_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Info Boxes ----

    ## Total number of youth (post filters) ----
    n_youth <- shiny::reactive(

      clients_filtered() |>
        nrow()

    )

    ## Filter income data ----
    income_data_filtered <- shiny::reactive({
      filter_data(income_data, clients_filtered())
    })

    most_recent_income_data_per_enrollment <- shiny::reactive({
      income_data_filtered() |> 
        filter_most_recent_data_per_enrollment()
    })

    ## Filter benefits data ----
    benefits_data_filtered <- shiny::reactive({
      filter_data(benefits_data, clients_filtered())
    })

    most_recent_benefits_data_per_enrollment <- shiny::reactive({
      benefits_data_filtered() |> 
        filter_most_recent_data_per_enrollment()
    })

    ## Filtered number of youth with income data ----
    n_youth_with_income_data <- shiny::reactive(

      income_data_filtered() |>
        dplyr::filter(
          !is.na(total_monthly_income)
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    ## Filtered number of youth with benefits data ----
    n_youth_with_benefits_data <- shiny::reactive(

      benefits_data_filtered() |>
        dplyr::filter(
          benefits_from_any_source %in% c("Yes", "No")
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    ## Render "number of youth with income data" info box ----
    output$n_youth_with_income_data <- shiny::renderText({
      n_youth_with_income_data()
    })

    ## Render "number of youth with benefits data" info box ----
    output$n_youth_with_benefits_data <- shiny::renderText({
      n_youth_with_benefits_data()
    })

    ## Income Chart ----
    output$income_chart <- echarts4r::renderEcharts4r(
      most_recent_income_data_per_enrollment() |>
        dplyr::count(income_from_any_source, .drop = FALSE) |>
        bar_chart(
          x = "income_from_any_source",
          y = "n"
        )
    )

    ## Income Source Chart ----
    output$income_source_chart <- echarts4r::renderEcharts4r({

      youth_with_income_from_any_source <- most_recent_income_data_per_enrollment() |>
        dplyr::filter(income_from_any_source == "Yes")
        
      youth_with_income_from_any_source |>
        dplyr::select(
          enrollment_id,
          organization_id,
          personal_id,
          earned,
          unemployment,
          ssi,
          ssdi,
          va_disability_service,
          va_disability_non_service,
          private_disability,
          workers_comp,
          tanf,
          ga,
          soc_sec_retirement,
          pension,
          child_support,
          alimony,
          other_income_source
        ) |>
        tidyr::pivot_longer(
          cols = earned:other_income_source,
          names_to = "income_source",
          values_to = "response"
        ) |>
        dplyr::filter(response == "Yes") |> 
        dplyr::mutate(
          income_source = factor(
            income_source,
            levels = c(
              "other_income_source",
              "alimony",
              "child_support",
              "pension",
              "soc_sec_retirement",
              "ga",
              "tanf",
              "workers_comp",
              "private_disability",
              "va_disability_non_service",
              "va_disability_service",
              "ssdi",
              "ssi",
              "unemployment",
              "earned"
            ),
            labels = c(
              "Other",
              "Alimony",
              "Child Support",
              "Pension",
              "Social Security Retirement",
              "GA",
              "TANF",
              "Workers Comp",
              "Private Disability",
              "VA Disability Non Service",
              "VA Disability Service",
              "SSDI",
              "SSI",
              "Unemployment",
              "Earned"
            ),
            ordered = TRUE
          )
        ) |>
        dplyr::count(income_source, .drop = FALSE) |>
        bar_chart(
          x = "income_source",
          y = "n",
          pct_denominator = nrow(youth_with_income_from_any_source)
        )
    })

    ## Monthly Income Chart ----
    output$income_bar_chart <- echarts4r::renderEcharts4r({
      youth_with_income_from_any_source <- most_recent_income_data_per_enrollment() |>
        dplyr::filter(income_from_any_source == "Yes")

      youth_with_income_from_any_source |>
        dplyr::count(total_monthly_income_grouped, .drop = FALSE) |>
        bar_chart(
          x = "total_monthly_income_grouped",
          y = "n"
        )
    })

    ## Benefits Chart ----
    output$benefits_chart <- echarts4r::renderEcharts4r({
      most_recent_benefits_data_per_enrollment() |>
        dplyr::count(benefits_from_any_source, .drop = FALSE) |> 
        bar_chart(
          x = "benefits_from_any_source",
          y = "n"
        )
    })

    ## Benefits Source Chart ----
    output$benefits_source_chart <- echarts4r::renderEcharts4r({

      youth_with_benefits_from_any_source <- most_recent_benefits_data_per_enrollment() |>
        dplyr::filter(benefits_from_any_source == "Yes")
        
      youth_with_benefits_from_any_source |>
        dplyr::select(
          enrollment_id,
          organization_id,
          snap,
          wic,
          tanf_child_care,
          tanf_transportation,
          other_tanf,
          other_benefits_source
        ) |>
        tidyr::pivot_longer(
          cols = snap:other_benefits_source,
          names_to = "benefits_source",
          values_to = "response"
        ) |>
        dplyr::filter(response == "Yes") |> 
        dplyr::mutate(
          benefits_source = factor(
            benefits_source,
            levels = c(
              "other_benefits_source",
              "other_tanf",
              "tanf_transportation",
              "tanf_child_care",
              "wic",
              "snap"
            ),
            labels = c(
              "Other",
              "Other TANF",
              "TANF Transportation",
              "TANF Child Care",
              "WIC",
              "Snap"
            ),
            ordered = TRUE
          )
        ) |>
        dplyr::count(benefits_source, .drop = FALSE) |>
        bar_chart(
          x = "benefits_source",
          y = "n",
          pct_denominator = nrow(youth_with_benefits_from_any_source)
        )

    })

    ## Benefits Sankey Chart ----
    output$benefits_sankey_chart <- echarts4r::renderEcharts4r(
      benefits_data_filtered() |>
        prepare_sankey_data(
          response_col = "benefits_from_any_source",
          response_vals = c("Yes", "No")
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    )

    ## Health Insurance Chart ----
    output$insurance_chart <- echarts4r::renderEcharts4r({
      most_recent_benefits_data_per_enrollment() |>
        dplyr::count(insurance_from_any_source, .drop = FALSE) |>
        bar_chart(
          x = "insurance_from_any_source",
          y = "n"
        )
    })

    ## Health Insurance Source Chart ----
    output$insurance_source_chart <- echarts4r::renderEcharts4r({

      youth_with_insurance_from_any_source <- most_recent_benefits_data_per_enrollment() |>
        dplyr::filter(insurance_from_any_source == "Yes")
        
      youth_with_insurance_from_any_source |>
        dplyr::select(
          enrollment_id,
          organization_id,
          personal_id,
          medicaid,
          medicare,
          schip,
          vha_services,
          employer_provided,
          cobra,
          private_pay,
          state_health_ins,
          indian_health_services,
          other_insurance
        ) |>
        tidyr::pivot_longer(
          cols = medicaid:other_insurance,
          names_to = "insurance_source",
          values_to = "response"
        ) |>
        dplyr::filter(response == "Yes") |> 
        dplyr::mutate(
          insurance_source = factor(
            insurance_source,
            levels = c(
              "other_insurance",
              "indian_health_services",
              "state_health_ins",
              "private_pay",
              "cobra",
              "employer_provided",
              "vha_services",
              "schip",
              "medicare",
              "medicaid"
            ),
            labels = c(
              "Other",
              "Indian Health Services",
              "State Health Insurance",
              "Private Pay",
              "Cobra",
              "Employer Provided",
              "VHA Services",
              "SCHIP",
              "Medicare",
              "Medicaid"
            ),
            ordered = TRUE
          )
        ) |>
        dplyr::count(insurance_source, .drop = FALSE) |>
        bar_chart(
          x = "insurance_source",
          y = "n",
          pct_denominator = nrow(youth_with_insurance_from_any_source)
        )
    })

    ## Health Insurance Sankey Chart ----
    output$insurance_sankey_chart <- echarts4r::renderEcharts4r(
      benefits_data_filtered() |>
        prepare_sankey_data(
          response_col = "insurance_from_any_source",
          response_vals = c("Yes", "No")
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    )

  })
}

## To be copied in the UI
# mod_income_benefits_ui("income_benefits_1")

## To be copied in the server
# mod_income_benefits_server("income_benefits_1")
