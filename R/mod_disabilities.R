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
        # Number of projects (post filters)
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_disabilities_data_box"),
          width = "100%"
        )
      ),

      shiny::column(
        width = 4,
        # Number of youth with no disabilities
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_no_disabilities_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    # Pie Charts ----

    shiny::fluidRow(

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = "# of Youth with Disabilities by Type",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("disabilities_pie_chart"),
            height = "350px"
          )
        )

      ),

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = "# of Youth with Substance Use by Type",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("substance_pie_chart"),
            height = "350px"
          )
        )

      )

    ),

    # Sankey Charts ----

    shiny::fluidRow(

      shiny::column(
        width = 12,

        bs4Dash::tabBox(
          title = "Changes in Disability Status (Entry --> Exit)",
          type = "tabs",
          side = "right",
          width = NULL,
          maximizable = TRUE,

          shiny::tabPanel(
            title = "Physical",
            echarts4r::echarts4rOutput(
              outputId = ns("physical_sankey_chart"),
              height = "350px"
            )
          ),

          shiny::tabPanel(
            title = "Developmental",
            echarts4r::echarts4rOutput(
              outputId = ns("developmental_sankey_chart"),
              height = "350px"
            )
          ),

          shiny::tabPanel(
            title = "Chronic",
            echarts4r::echarts4rOutput(
              outputId = ns("chronic_sankey_chart"),
              height = "350px"
            )
          ),

          shiny::tabPanel(
            title = "HIV/AIDS",
            echarts4r::echarts4rOutput(
              outputId = ns("hiv_sankey_chart"),
              height = "350px"
            )
          ),

          shiny::tabPanel(
            title = "Mental",
            echarts4r::echarts4rOutput(
              outputId = ns("mental_sankey_chart"),
              height = "350px"
            )
          ),

          shiny::tabPanel(
            title = "Substance Use",
            echarts4r::echarts4rOutput(
              outputId = ns("substance_sankey_chart"),
              height = "350px"
            )
          )

        )

      )

    ),

    # Data Quality Stats ----

    shiny::fluidRow(
      shiny::column(
        width = 12,

        bs4Dash::box(
          title = "Data Quality Statistics",
          width = NULL,
          maximizable = TRUE,
          reactable::reactableOutput(
            outputId = ns("missingness_stats_tbl")
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

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the disabilities data
    disabilities_data_filtered <- shiny::reactive({

      disabilities_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    })

    # Total number of Youth in program(s) that exist in the `disabilities.csv`
    # file
    n_youth_with_disabilities_data <- shiny::reactive(

      disabilities_data_filtered() |>
        dplyr::filter(
          disability_response %in% c(
            "Yes", "No", SubstanceUseDisorderCodes$Description[2:4]
          )
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
    output$n_youth_with_disabilities_data_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_disabilities_data(),
        subtitle = "Total # of Youth with Disabilities Data Available",
        icon = shiny::icon("home")
      )

    })

    # Create reactive count of the number of youth with no disabilities
    n_youth_with_no_disabilities <- shiny::reactive(

      disabilities_data_filtered() |>
        dplyr::distinct(organization_id, personal_id, disability_response) |>
        dplyr::group_by(organization_id, personal_id) |>
        dplyr::filter(dplyr::n() == 1L & disability_response == "No") |>
        dplyr::ungroup() |>
        nrow()

    )

    # Render number of youth with no disabilities box
    output$n_youth_with_no_disabilities_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_no_disabilities(),
        subtitle = "Total # of Youth with No Disabilities or Substance Use",
        icon = shiny::icon("home")
      )

    })

    # Pie Charts ----

    ## Disabilities Pie Chart ----

    # Create reactive data frame to data to be displayed in pie chart
    disabilities_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(disabilities_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- disabilities_data_filtered() |>
        dplyr::filter(disability_response == "Yes") |>
        dplyr::arrange(
          organization_id,
          personal_id,
          disability_type,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          disability_type,
          disability_response
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          disability_type,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(disability_type) |>
        dplyr::arrange(disability_type)

    })

    # Create disabilities pie chart
    output$disabilities_pie_chart <- echarts4r::renderEcharts4r({

      disabilities_pie_chart_data() |>
        pie_chart(
          category = "disability_type",
          count = "n"
        )

    })

    ## Substance Use Disorder Pie Chart ----

    # Create reactive data frame to data to be displayed in pie chart
    substance_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(disabilities_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- disabilities_data_filtered() |>
        dplyr::filter(
          disability_response %in% SubstanceUseDisorderCodes$Description[2:4]
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          disability_response
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
        dplyr::count(disability_response) |>
        dplyr::arrange(disability_response)

    })

    # Create substance use pie chart
    output$substance_pie_chart <- echarts4r::renderEcharts4r({

      substance_pie_chart_data() |>
        pie_chart(
          category = "disability_response",
          count = "n"
        )

    })

    # Sankey Charts ----

    ## Physical Disabilities ----

    # Create reactive data frame to data to be displayed in line chart
    physical_sankey_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(disabilities_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      ids_exited <- disabilities_data_filtered() |>
        dplyr::filter(
          disability_type == "Physical Disability",
          disability_response %in% c("Yes", "No")
        ) |>
        get_ids_for_sankey()

      shiny::validate(
        shiny::need(
          expr = nrow(ids_exited) >= 1L,
          message = "No data to display"
        )
      )

      disabilities_data_filtered() |>
        dplyr::filter(
          disability_type == "Physical Disability",
          disability_response %in% c("Yes", "No")
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = disability_response)

    })

    # Create physical sankey chart
    output$physical_sankey_chart <- echarts4r::renderEcharts4r({

      physical_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    })

    ## Developmental Disabilities ----

    # Create reactive data frame to data to be displayed in line chart
    developmental_sankey_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(disabilities_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      ids_exited <- disabilities_data_filtered() |>
        dplyr::filter(
          disability_type == "Developmental Disability",
          disability_response %in% c("Yes", "No")
        ) |>
        get_ids_for_sankey()

      shiny::validate(
        shiny::need(
          expr = nrow(ids_exited) >= 1L,
          message = "No data to display"
        )
      )

      disabilities_data_filtered() |>
        dplyr::filter(
          disability_type == "Developmental Disability",
          disability_response %in% c("Yes", "No")
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = disability_response)

    })

    # Create developmental sankey chart
    output$developmental_sankey_chart <- echarts4r::renderEcharts4r({

      developmental_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    })

    ## Chronic Health Condition ----

    # Create reactive data frame to data to be displayed in line chart
    chronic_sankey_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(disabilities_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      ids_exited <- disabilities_data_filtered() |>
        dplyr::filter(
          disability_type == "Chronic Health Condition",
          disability_response %in% c("Yes", "No")
        ) |>
        get_ids_for_sankey()

      shiny::validate(
        shiny::need(
          expr = nrow(ids_exited) >= 1L,
          message = "No data to display"
        )
      )

      disabilities_data_filtered() |>
        dplyr::filter(
          disability_type == "Chronic Health Condition",
          disability_response %in% c("Yes", "No")
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = disability_response)

    })

    # Create chronic sankey chart
    output$chronic_sankey_chart <- echarts4r::renderEcharts4r({

      chronic_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    })

    ## HIV/AIDS ----

    # Create reactive data frame to data to be displayed in line chart
    hiv_sankey_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(disabilities_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      ids_exited <- disabilities_data_filtered() |>
        dplyr::filter(
          disability_type == "HIV/AIDS",
          disability_response %in% c("Yes", "No")
        ) |>
        get_ids_for_sankey()

      shiny::validate(
        shiny::need(
          expr = nrow(ids_exited) >= 1L,
          message = "No data to display"
        )
      )

      disabilities_data_filtered() |>
        dplyr::filter(
          disability_type == "HIV/AIDS",
          disability_response %in% c("Yes", "No")
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = disability_response)

    })

    # Create chronic sankey chart
    output$hiv_sankey_chart <- echarts4r::renderEcharts4r({

      hiv_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    })

    ## Mental Health Disorder ----

    # Create reactive data frame to data to be displayed in line chart
    mental_sankey_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(disabilities_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      ids_exited <- disabilities_data_filtered() |>
        dplyr::filter(
          disability_type == "Mental Health Disorder",
          disability_response %in% c("Yes", "No")
        ) |>
        get_ids_for_sankey()

      shiny::validate(
        shiny::need(
          expr = nrow(ids_exited) >= 1L,
          message = "No data to display"
        )
      )

      disabilities_data_filtered() |>
        dplyr::filter(
          disability_type == "Mental Health Disorder",
          disability_response %in% c("Yes", "No")
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = disability_response)

    })

    # Create disabilities trend line chart
    output$mental_sankey_chart <- echarts4r::renderEcharts4r({

      mental_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    })

    ## Substance Use Disorder ----

    # Create reactive data frame to data to be displayed in line chart
    substance_sankey_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(disabilities_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      ids_exited <- disabilities_data_filtered() |>
        dplyr::filter(
          disability_type == "Substance Use Disorder",
          disability_response %in% SubstanceUseDisorderCodes$Description[1:4]
        ) |>
        get_ids_for_sankey()

      shiny::validate(
        shiny::need(
          expr = nrow(ids_exited) >= 1L,
          message = "No data to display"
        )
      )

      disabilities_data_filtered() |>
        dplyr::filter(
          disability_type == "Substance Use Disorder",
          disability_response %in% SubstanceUseDisorderCodes$Description[1:4]
        ) |>
        # Recode 'disability_response' to either "Yes" or "No"
        dplyr::mutate(
          disability_response = dplyr::case_when(
            disability_response == "No" ~ "No",
            TRUE ~ "Yes"
          )
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = disability_response)

    })

    # Create substance trend line chart
    output$substance_sankey_chart <- echarts4r::renderEcharts4r({

      substance_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    })

    # Missingness Statistics ----
    missingness_stats <- shiny::reactive({

      disabilities_data_filtered() |>
        dplyr::mutate(disability_response = ifelse(
          is.na(disability_response),
          "(Blank)",
          disability_response
        )) |>
        dplyr::filter(disability_response %in% c(
          "Client doesn't know",
          "Client prefers not to answer",
          "Data not collected",
          "(Blank)"
        )) |>
        dplyr::count(disability_response, name = "Count") |>
        dplyr::rename(Response = disability_response)

    })

    output$missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        missingness_stats()
      )
    )

  })
}

## To be copied in the UI
# mod_disabilities_ui("disabilities_1")

## To be copied in the server
# mod_disabilities_server("disabilities_1")
