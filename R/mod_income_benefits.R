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

                ### Income Pie Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Income Received (from Any Source)",
                    content = link_section("4.02 Income and Sources")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("income_pie_chart"),
                    height = "100%"
                  )
                )

              ),

              shiny::column(
                width = 6,

                ### Income Source Pie Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Income Received by Source (# of Youth)",
                    content = link_section("4.02 Income and Sources")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("income_source_pie_chart"),
                    height = "100%"
                  )
                )

              )

            ),

            shiny::fluidRow(

              shiny::column(
                width = 12,

                ### Income Bar Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Total Monthly Income (# of Youth)",
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

            ),

            shiny::fluidRow(

              shiny::column(
                width = 12,

                ### Benefits Data Quality Table ----
                bs4Dash::box(
                  title = "Income Data Quality Statistics",
                  width = NULL,
                  maximizable = TRUE,
                  reactable::reactableOutput(
                    outputId = ns("income_missingness_stats_tbl")
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
                    outputId = ns("benefits_pie_chart"),
                    height = "100%"
                  )
                )

              ),

              shiny::column(
                width = 6,

                ### Benefits Source Pie Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Benefits Received by Source",
                    content = link_section("4.03 Non-Cash Benefits")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("benefits_source_pie_chart"),
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
                  title = with_popover(
                    text = "Health Insurance Received (from Any Source)",
                    content = link_section("4.04 Health Insurance")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("insurance_pie_chart"),
                    height = "100%"
                  )
                )

              ),

              shiny::column(
                width = 6,

                ### Health Insurance Source Pie Chart ----
                bs4Dash::box(
                  title = with_popover(
                    text = "Health Insurance Received by Source",
                    content = link_section("4.04 Health Insurance")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("insurance_source_pie_chart"),
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

    ## Income Pie Chart ----
    output$income_pie_chart <- echarts4r::renderEcharts4r(
      most_recent_income_data_per_enrollment() |>
        dplyr::filter(income_from_any_source %in% c("Yes", "No")) |>
        dplyr::count(income_from_any_source) |>
        pie_chart(
          category = "income_from_any_source",
          count = "n"
        )
    )

    ## Income Bar Chart ----
    output$income_bar_chart <- echarts4r::renderEcharts4r(
      most_recent_income_data_per_enrollment() |>
        dplyr::filter(!is.na(total_monthly_income_grouped)) |>
        dplyr::count(total_monthly_income_grouped, .drop = FALSE) |>
        echarts4r::e_charts(x = total_monthly_income_grouped) |>
        echarts4r::e_bar(
          serie = n,
          name = "# of Youth",
          legend = FALSE,
          label = list(
            formatter = "{@[0]}",
            show = TRUE,
            position = "right"
          )
        ) |>
        echarts4r::e_tooltip(trigger = "item") |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_grid(containLabel = TRUE) |>
        echarts4r::e_show_loading()
    )

    ## Benefits Pie Chart ----
    output$benefits_pie_chart <- echarts4r::renderEcharts4r({
      most_recent_benefits_data_per_enrollment() |>
        dplyr::filter(benefits_from_any_source %in% c("Yes", "No")) |>
        dplyr::count(benefits_from_any_source) |> 
        pie_chart(
          category = "benefits_from_any_source",
          count = "n"
        )
    })

    ## Health Insurance Pie Chart ----
    output$insurance_pie_chart <- echarts4r::renderEcharts4r({
      most_recent_benefits_data_per_enrollment() |>
        dplyr::filter(insurance_from_any_source %in% c("Yes", "No")) |>
        dplyr::count(insurance_from_any_source) |>
        pie_chart(
          category = "insurance_from_any_source",
          count = "n"
        )
    })

    ## Income Source Pie Chart ----
    output$income_source_pie_chart <- echarts4r::renderEcharts4r({
      most_recent_income_data_per_enrollment() |>
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
        dplyr::count(income_source) |>
        dplyr::mutate(
          income_source = dplyr::recode(
            income_source,
            "earned" = "Earned",
            "unemployment" = "Unemployment",
            "ssi" = "SSI",
            "ssdi" = "SSDI",
            "va_disability_service" = "VA Disability Service",
            "va_disability_non_service" = "VA Disability Non Service",
            "private_disability" = "Private Disability",
            "workers_comp" = "Workers Comp",
            "tanf" = "TANF",
            "ga" = "GA",
            "soc_sec_retirement" = "Social Security Retirement",
            "pension" = "Pension",
            "child_support" = "Child Support",
            "alimony" = "Alimony",
            "other_income_source" = "Other"
          )
        ) |>
        pie_chart(
          category = "income_source",
          count = "n"
        )
    })

    ## Benefits Source Pie Chart ----
    output$benefits_source_pie_chart <- echarts4r::renderEcharts4r({
      most_recent_benefits_data_per_enrollment() |>
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
        dplyr::count(benefits_source) |>
        dplyr::mutate(
          benefits_source = dplyr::recode(
            benefits_source,
            "snap" = "Snap",
            "wic" = "WIC",
            "tanf_child_care" = "TANF Child Care",
            "tanf_transportation" = "TANF Transportation",
            "other_tanf" = "Other TANF",
            "other_benefits_source" = "Other"
          )
        ) |>
        pie_chart(
          category = "benefits_source",
          count = "n"
        )
    })

    ## Health Insurance Source Pie Chart ----
    output$insurance_source_pie_chart <- echarts4r::renderEcharts4r({
      most_recent_benefits_data_per_enrollment() |>
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
        dplyr::count(insurance_source) |>
        dplyr::mutate(
          insurance_source = dplyr::recode(
            insurance_source,
            "medicaid" = "Medicaid",
            "medicare" = "Medicare",
            "schip" = "SCHIP",
            "vha_services" = "VHA Services",
            "employer_provided" = "Employer Provided",
            "cobra" = "Cobra",
            "private_pay" = "Private Pay",
            "state_health_ins" = "State Health Insurance",
            "indian_health_services" = "Indian Health Services",
            "other_insurance" = "Other"
          )
        ) |>
        pie_chart(
          category = "insurance_source",
          count = "n"
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

    ## Data Quality Statistics ----

    ### Get data for income data quality table ----
    income_missingness_stats <- shiny::reactive(

      income_data_filtered() |>
        dplyr::mutate(income_from_any_source = ifelse(
          is.na(income_from_any_source),
          "(Blank)",
          income_from_any_source
        )) |>
        dplyr::filter(
          !income_from_any_source %in% c("Yes", "No")
        ) |>
        dplyr::count(income_from_any_source, name = "Count") |>
        dplyr::rename(Response = income_from_any_source)

    )

    ### Render income data quality table ----
    output$income_missingness_stats_tbl <- reactable::renderReactable(

      reactable::reactable(
        income_missingness_stats()
      )

    )

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
# mod_income_benefits_ui("income_benefits_1")

## To be copied in the server
# mod_income_benefits_server("income_benefits_1")
