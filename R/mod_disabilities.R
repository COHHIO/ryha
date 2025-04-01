#' disabilities UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_disabilities_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Boxes ----

    shiny::fluidRow(

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth")),
        subtitle = "Total # of Youth in Program(s)",

        ## TODO: Implement these "improved" counts
        ## Number of youth in disabilities data (post filters)
        ## This number corresponds to the number of youth in disability table,
        ##   regardless of data quality (youth with data not collected in all
        ##   disability columns is still counted here)
        # value = shiny::textOutput(outputId = ns("n_youth_in_disability_data")),
        # subtitle = "Total # of Youth in Disabilities Data"

        icon = shiny::icon("user", class = "fa-solid"),
        width = 4
      ),

      # Number of youth with disabilities or substance use (post filters)
      # This number corresponds to youth that have a value of "Yes" in one
      # of the columns that refer to disabilities or substance use. In case
      # the youth has multiple entries, the most recent value will be used.
      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_disabilities_or_substance_use")),
        subtitle = "Total # of Youth with Disabilities or Substance Use",
        icon = shiny::icon("accessible-icon"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_without_disabilities_or_substance_use")),
        subtitle = "Total # of Youth without Informed Disabilities or Substance Use",
        icon = shiny::icon("accessible-icon"),
        width = 4
      )

    ),

    shiny::hr(),

    # Charts ----

    shiny::fluidRow(

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = with_popover(
            text = "Disability Prevalence in Youth",
            content = shiny::tagList(
              shiny::span("Each bar summarizes the responses for the corresponding disability."),
              shiny::br(),
              shiny::span("Youth with multiple disabilities are counted once per disability."),
              shiny::br(),
              shiny::span("For more information, refer to sections:"),
              shiny::tags$ul(
                shiny::tags$li(shiny::tags$b("4.05 Physical Disability")),
                shiny::tags$li(shiny::tags$b("4.06 Developmental Disability")),
                shiny::tags$li(shiny::tags$b("4.07 Chronic Health Condition")),
                shiny::tags$li(shiny::tags$b("4.08 HIV/AIDS")),
                shiny::tags$li(shiny::tags$b("4.09 Mental Health Disorder"))
              ),
              shiny::span("in the ", link_data_standards_manual())
            )
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("disabilities_chart"),
            height = "100%"
          )
        )

      ),

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = with_popover(
            text = "# of Youth by Substance Use",
            content = link_section("4.10 Substance Use Disorder")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("substance_chart"),
            height = "100%"
          )
        )

      )

    ),

    # Sankey Charts ----

    shiny::fluidRow(

      shiny::column(
        width = 12,

        bs4Dash::tabBox(
          title = with_popover(
            text = "Changes in Disability Status (Entry --> Exit)",
            content = shiny::tagList(
              shiny::span("For more information, refer to sections:"),
              shiny::tags$ul(
                shiny::tags$li(shiny::tags$b("4.05 Physical Disability")),
                shiny::tags$li(shiny::tags$b("4.06 Developmental Disability")),
                shiny::tags$li(shiny::tags$b("4.07 Chronic Health Condition")),
                shiny::tags$li(shiny::tags$b("4.08 HIV/AIDS")),
                shiny::tags$li(shiny::tags$b("4.09 Mental Health Disorder"))
              ),
              shiny::span("in the ", link_data_standards_manual())
            )
          ),
          type = "tabs",
          side = "right",
          height = DEFAULT_BOX_HEIGHT,
          width = NULL,
          maximizable = TRUE,

          shiny::tabPanel(
            title = "Physical",
            echarts4r::echarts4rOutput(
              outputId = ns("physical_sankey_chart"),
              height = "100%"
            )
          ),

          shiny::tabPanel(
            title = "Developmental",
            echarts4r::echarts4rOutput(
              outputId = ns("developmental_sankey_chart"),
              height = "100%"
            )
          ),

          shiny::tabPanel(
            title = "Chronic",
            echarts4r::echarts4rOutput(
              outputId = ns("chronic_sankey_chart"),
              height = "100%"
            )
          ),

          shiny::tabPanel(
            title = "HIV/AIDS",
            echarts4r::echarts4rOutput(
              outputId = ns("hiv_sankey_chart"),
              height = "100%"
            )
          ),

          shiny::tabPanel(
            title = "Mental",
            echarts4r::echarts4rOutput(
              outputId = ns("mental_sankey_chart"),
              height = "100%"
            )
          ),

          shiny::tabPanel(
            title = "Substance Use",
            echarts4r::echarts4rOutput(
              outputId = ns("substance_sankey_chart"),
              height = "100%"
            )
          )

        ) |>
          shiny::tagAppendAttributes(class = "sankey-tabset")

      )

    ),

    # Data Quality Stats ----

    shiny::fluidRow(
      shiny::column(
        width = 12,

        bs4Dash::tabBox(
          title = "Data Quality Statistics",
          type = "tabs",
          side = "right",
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,

          # shiny::tabPanel(
          #   title = "Summary",
          #   shiny::htmlOutput(ns("data_quality_string")),
          # ),

          shiny::tabPanel(
            title = "Youth by Number of Answers Missing",
            reactable::reactableOutput(outputId = ns("missingness_stats_tbl1")),
            shiny::br(),
            shiny::em("Note: \"Missing\" is defined as \"Client doesn't know\", \"Client prefers not to answer\", \"Data not collected\", or blank.")
          )

        )

      )
    )

  )
}

#' disabilities Server Functions
#'
#' @noRd
mod_disabilities_server <- function(id, disabilities_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    # Filter disabilities data
    # disabilities_data_filtered has one row per enrollment, data collection stage and disability type
    disabilities_data_filtered <- shiny::reactive({
      filter_data(disabilities_data, clients_filtered())
    })

    # Create reactive with the most recent data collected per enrollment
    most_recent_data_per_enrollment <- shiny::reactive({
      out <- disabilities_data_filtered() |>
        tidyr::pivot_wider(names_from = disability_type, values_from = disability_response) |>
        filter_most_recent_data_per_enrollment()

      # Handle missing responses
      missing_columns <- setdiff(
        c("Chronic Health Condition",
          "Developmental Disability",
          "HIV/AIDS",
          "Mental Health Disorder",
          "Physical Disability",
          "Substance Use Disorder"),
        colnames(out)
      )

      if (length(missing_columns) > 0) {
        for (column in missing_columns) {
          out[[column]] <- NA_character_
        }
      }

      out
    })

    # TODO // Implement these "improved" counts across the first infoBox for
    # all pages

    n_youth_in_disability_data <- reactive({

      # TODO: Same as nrow(most_recent_data_per_enrollment())
      disabilities_data_filtered() |>
        dplyr::select(organization_id, personal_id) |>
        dplyr::n_distinct()

    })

    # # Render number of youth in disabilities data
    # output$n_youth_in_disability_data <- shiny::renderText({
    #   n_youth_in_disability_data(),
    # })

    # Total number of youth with disabilities or substance use
    n_youth_with_disabilities_or_substance_use <- shiny::reactive({

      most_recent_data_per_enrollment() |>
        dplyr::filter(
          rowSums(
            dplyr::across(
              .cols = c(
                `Physical Disability`,
                `Developmental Disability`,
                `Chronic Health Condition`,
                `HIV/AIDS`,
                `Mental Health Disorder`,
                `Substance Use Disorder`
              ),
              .fns = function(value) value == "Yes"
            )
          ) > 0
        ) |>
        nrow()

    })

    # Render number of youth with disabilities or substance use box value
    output$n_youth_with_disabilities_or_substance_use <- shiny::renderText({
      n_youth_with_disabilities_or_substance_use()
    })

    # Create reactive count of the number of youth without disabilities or substance use
    # We are counting number of youth without a "Yes" in any column, so any missing data
    # is counted as a "No"
    n_youth_without_disabilities_or_substance_use <- shiny::reactive(

      n_youth_in_disability_data() - n_youth_with_disabilities_or_substance_use()

    )

    # Render number of youth with no disabilities box value
    output$n_youth_without_disabilities_or_substance_use <- shiny::renderText({
      n_youth_without_disabilities_or_substance_use()
    })

    # Charts ----

    ## Disabilities Chart ----

    # Create reactive data frame to data to be displayed in chart
    disabilities_chart_data <- shiny::reactive({
      most_recent_data_per_enrollment() |>
        # Remove Substance Use Disorder column
        dplyr::select(-`Substance Use Disorder`) |>
        # Each disability had its own column, we need data to be in long format
        tidyr::pivot_longer(
          cols = c(
            `Physical Disability`,
            `Developmental Disability`,
            `Chronic Health Condition`,
            `HIV/AIDS`,
            `Mental Health Disorder`
          ),
          names_to = "Disability",
          values_to = "Response"
        ) |>
        # We will consider NA values as "Missing"
        tidyr::replace_na(list(Response = "Missing")) |>
        dplyr::mutate(
          Response = factor(
            Response,
            levels = c(
              "Yes",
              "No",
              "Client doesn't know",
              "Client prefers not to answer",
              "Data not collected",
              "Missing"
            ),
            ordered = TRUE
          )
        ) |>
        dplyr::count(Disability, Response, .drop = FALSE) |>
        dplyr::group_by(Disability) |>
        dplyr::mutate(pct = round(n / sum(n), 4)) |>
        dplyr::ungroup() |>
        # Order response categories
        dplyr::group_by(Response)

    })

    # Create disabilities chart
    output$disabilities_chart <- echarts4r::renderEcharts4r({

      disabilities_chart_data() |>
        echarts4r::e_chart(x = Disability) |>
        echarts4r::e_bar(serie = n, stack = "my_stack") |>
        echarts4r::e_add_nested('extra', pct) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_grid(containLabel = TRUE) |>
        echarts4r::e_color(
          c(
           COLORS$YES_NEUTRAL,   # "Yes",
           COLORS$NO_NEUTRAL,   # "No",
           COLORS$CLIENT_DOESNT_KNOW,   # "Client doesn't know",
           COLORS$CLIENT_PREFERS_NOT_TO_ANSWER,   # "Client prefers not to answer",
           COLORS$DATA_NOT_COLLECTED,   # "Data not collected",
           COLORS$MISSING   # "Missing"
          )
        ) |>
        add_stacked_bar_tooltip()
    })

    ## Substance Use Disorder Chart ----

    # Create substance use pie chart
    output$substance_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |>
        tidyr::replace_na(list(`Substance Use Disorder` = "Missing")) |>
        dplyr::mutate(
          `Substance Use Disorder` = factor(
          `Substance Use Disorder`,
            levels = c(
              "Missing",
              "Data not collected",
              "Client prefers not to answer",
              "Client doesn't know",
              "No",
              "Both alcohol and drug use disorders",
              "Drug use disorder",
              "Alcohol use disorder"
            ),
            labels = c(
              "Missing",
              "Data not collected",
              "Client prefers not to answer",
              "Client doesn't know",
              "No substance use",
              "Both alcohol and drug use disorders",
              "Drug use disorder",
              "Alcohol use disorder"
            ),
            ordered = TRUE
          )  
        ) |>
        dplyr::count(`Substance Use Disorder`, .drop = FALSE) |> 
        bar_chart(
          x = "Substance Use Disorder",
          y = "n"
        )
    })

    # Sankey Charts ----
    ## Physical Disabilities ----
    output$physical_sankey_chart <- echarts4r::renderEcharts4r({
      disabilities_data_filtered() |> 
        dplyr::filter(disability_type == "Physical Disability") |> 
        prepare_sankey_data(
          response_col = "disability_response",
          response_vals = c("Yes", "No")
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

    ## Developmental Disabilities ----
    output$developmental_sankey_chart <- echarts4r::renderEcharts4r({
      disabilities_data_filtered() |> 
        dplyr::filter(disability_type == "Developmental Disability") |> 
        prepare_sankey_data(
          response_col = "disability_response",
          response_vals = c("Yes", "No")
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

    ## Chronic Health Condition ----
    output$chronic_sankey_chart <- echarts4r::renderEcharts4r({
      disabilities_data_filtered() |> 
        dplyr::filter(disability_type == "Chronic Health Condition") |> 
        prepare_sankey_data(
          response_col = "disability_response",
          response_vals = c("Yes", "No")
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

    ## HIV/AIDS ----
    output$hiv_sankey_chart <- echarts4r::renderEcharts4r({
      disabilities_data_filtered() |> 
        dplyr::filter(disability_type == "HIV/AIDS") |> 
        prepare_sankey_data(
          response_col = "disability_response",
          response_vals = c("Yes", "No")
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

    ## Mental Health Disorder ----
    output$mental_sankey_chart <- echarts4r::renderEcharts4r({
      disabilities_data_filtered() |> 
        dplyr::filter(disability_type == "Mental Health Disorder") |> 
        prepare_sankey_data(
          response_col = "disability_response",
          response_vals = c("Yes", "No")
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

    ## Substance Use Disorder ----
    output$substance_sankey_chart <- echarts4r::renderEcharts4r({
      disabilities_data_filtered() |> 
        dplyr::filter(disability_type == "Substance Use Disorder") |> 
        # Recode 'disability_response' to either "Yes" or "No"
        dplyr::mutate(
          disability_response = dplyr::case_when(
            disability_response == "No" ~ "No",
            disability_response %in% c(
              "Alcohol use disorder",
              "Drug use disorder",
              "Both alcohol and drug use disorders"
            ) ~ "Yes"
          )
        ) |>
        prepare_sankey_data(
          response_col = "disability_response",
          response_vals = c("Yes", "No")
        ) |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )
    })

    # Missingness Statistics ----
    # output$data_quality_string <- shiny::renderUI({
    #   glue::glue(
    #     "<strong>{round(n_youth_in_disability_data()/n_youth(), 2) * 100}%</strong>
    #     of youth in selected program(s) have entries in Disabilities Data."
    #   ) |>
    #     shiny::HTML()
    # })

    output$missingness_stats_tbl1 <- reactable::renderReactable(

      most_recent_data_per_enrollment() |>
        dplyr::mutate(
          `Answers Missing` = rowSums(
            dplyr::across(
              .cols = c(
                `Physical Disability`,
                `Developmental Disability`,
                `Chronic Health Condition`,
                `HIV/AIDS`,
                `Mental Health Disorder`,
                `Substance Use Disorder`
              ),
              .fns = function(value) !value %in% c("Yes", "No")
            )
          )
        ) |>
        dplyr::count(`Answers Missing`, name = "# Youth") |>
        reactable::reactable()

    )

  })
}

## To be copied in the UI
# mod_disabilities_ui("disabilities_1")

## To be copied in the server
# mod_disabilities_server("disabilities_1")
