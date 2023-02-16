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

      shiny::column(
        width = 4,
        ## Number of youth (post filters) ----
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_box"),
          width = "100%"
        )
      ),

      shiny::column(
        width = 4,
        ## Number of youth with income data (post filters) ----
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_income_data_box"),
          width = "100%"
        )
      ),

      shiny::column(
        width = 4,
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

          ## Income Tab Panel ----
          shiny::tabPanel(
            title = "Income",

            shiny::fluidRow(

              shiny::column(
                width = 6,

                ### Income Pie Chart ----
                bs4Dash::box(
                  title = "Income Received (from Any Source)",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("income_pie_chart"),
                    height = "400px"
                  )
                )

              ),

              shiny::column(
                width = 6,

                ### Income Source Pie Chart ----
                bs4Dash::box(
                  title = "Income Received by Source (# of Youth)",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("income_source_pie_chart"),
                    height = "400px"
                  )
                )

              )

            ),

            shiny::fluidRow(

              shiny::column(
                width = 12,

                ### Income Bar Chart ----
                bs4Dash::box(
                  title = "Income Received by Source ($ Amount)",
                  width = NULL,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("income_bar_chart"),
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

    ## Apply the filters to the income data ----
    income_data_filtered <- shiny::reactive({

      income_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    })

    ## Apply the filters to the benefits data ----
    benefits_data_filtered <- shiny::reactive({

      benefits_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

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

    ## Render "number of clients" info box ----
    output$n_youth_box <- bs4Dash::renderbs4ValueBox(

      bs4Dash::bs4ValueBox(
        value = n_youth(),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user")
      )

    )

    ## Render "number of youth with income data" info box ----
    output$n_youth_with_income_data_box <- bs4Dash::renderbs4ValueBox(

      bs4Dash::bs4ValueBox(
        value = n_youth_with_income_data(),
        subtitle = "Total # of Youth with Income Data Available",
        icon = shiny::icon("home")
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

    ## Income Pie Chart ----

    ### Get data for income pie chart ----
    # Create reactive data frame to data to be displayed in pie chart
    income_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(income_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- income_data_filtered() |>
        dplyr::filter(
          income_from_any_source %in% c("Yes", "No")
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          income_from_any_source,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          income_from_any_source
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          income_from_any_source,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(income_from_any_source) |>
        dplyr::arrange(income_from_any_source)

    })

    ### Render income pie chart ----
    output$income_pie_chart <- echarts4r::renderEcharts4r(

      income_pie_chart_data() |>
        pie_chart(
          category = "income_from_any_source",
          count = "n"
        )

    )

    ## Income Bar Chart ----

    # Create reactive data frame to data to be displayed in bar chart
    income_bar_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(income_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- income_data_filtered() |>
        dplyr::select(
          organization_id,
          personal_id,
          earned_amount:other_income_amount,
          date_updated
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          dplyr::ends_with("_amount", ignore.case = FALSE),
          date_updated
        ) |>
        tidyr::pivot_longer(
          cols = earned_amount:other_income_amount,
          names_to = "income_source",
          values_to = "amount"
        ) |>
        dplyr::filter(!is.na(amount)) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          income_source,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          income_source,
          amount
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          income_source,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::group_by(income_source) |>
        dplyr::summarise(amount = round(sum(amount), 0), .groups = "drop") |>
        dplyr::mutate(
          income_source = dplyr::case_when(
            income_source == "earned_amount" ~ "Earned",
            income_source == "unemployment_amount" ~ "Unemployment",
            income_source == "ssi_amount" ~ "SSI",
            income_source == "ssdi_amount" ~ "SSDI",
            income_source == "va_disability_service_amount" ~ "VA Disability Service",
            income_source == "va_disability_non_service_amount" ~ "VA Disability Non Service",
            income_source == "private_disability_amount" ~ "Private Disability",
            income_source == "workers_comp_amount" ~ "Workers Comp",
            income_source == "tanf_amount" ~ "TANF",
            income_source == "ga_amount" ~ "GA",
            income_source == "soc_sec_retirement_amount" ~ "Social Security Retirement",
            income_source == "pension_amount" ~ "Pension",
            income_source == "child_support_amount" ~ "Child Support",
            income_source == "alimony_amount" ~ "Alimony",
            income_source == "other_income_amount" ~ "Other"
          )
        ) |>
        dplyr::arrange(amount)

    })

    ### Render income bar chart ----
    output$income_bar_chart <- echarts4r::renderEcharts4r(

      income_bar_chart_data() |>
        echarts4r::e_charts(x = income_source) |>
        echarts4r::e_bar(
          serie = amount,
          name = "Amount Received",
          legend = FALSE,
          label = list(
            formatter = htmlwidgets::JS("
            function(params){
              return('$' + String(params.value[0]).replace(/\\B(?=(\\d{3})+(?!\\d))/g, ','))
            }
          "),
            show = TRUE,
            position = "right"
          )
        ) |>
        echarts4r::e_tooltip(
          formatter = htmlwidgets::JS("
            function(params){
              return('<strong>' + params.value[1] +
                     '</strong><br />' + params.marker + 'Amount Received: $' + String(params.value[0]).replace(/\\B(?=(\\d{3})+(?!\\d))/g, ','))
                     }
          ")
        ) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_x_axis(formatter = echarts4r::e_axis_formatter(style = "currency")) |>
        echarts4r::e_grid(containLabel = TRUE) |>
        echarts4r::e_show_loading()

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

    ## Income Source Pie Chart ----

    ### Get data for income source pie chart ----
    income_source_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(income_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- income_data_filtered() |>
        dplyr::select(
          organization_id,
          personal_id,
          earned:other_income_source,
          date_updated
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          !dplyr::ends_with("_amount", ignore.case = FALSE)
        ) |>
        tidyr::pivot_longer(
          cols = earned:other_income_source,
          names_to = "income_source",
          values_to = "response"
        ) |>
        dplyr::filter(response == "Yes") |>
        dplyr::arrange(
          organization_id,
          personal_id,
          income_source,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          income_source
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          income_source,
          .keep_all = TRUE
        )


      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(income_source) |>
        dplyr::mutate(
          income_source = dplyr::case_when(
            income_source == "earned" ~ "Earned",
            income_source == "unemployment" ~ "Unemployment",
            income_source == "ssi" ~ "SSI",
            income_source == "ssdi" ~ "SSDI",
            income_source == "va_disability_service" ~ "VA Disability Service",
            income_source == "va_disability_non_service" ~ "VA Disability Non Service",
            income_source == "private_disability" ~ "Private Disability",
            income_source == "workers_comp" ~ "Workers Comp",
            income_source == "tanf" ~ "TANF",
            income_source == "ga" ~ "GA",
            income_source == "soc_sec_retirement" ~ "Social Security Retirement",
            income_source == "pension" ~ "Pension",
            income_source == "child_support" ~ "Child Support",
            income_source == "alimony" ~ "Alimony",
            income_source == "other_income_source" ~ "Other"
          )
        )

    })

    ### Render income source pie chart ----
    output$income_source_pie_chart <- echarts4r::renderEcharts4r({

      income_source_pie_chart_data() |>
        pie_chart(
          category = "income_source",
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
