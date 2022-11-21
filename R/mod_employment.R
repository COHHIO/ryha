#' employment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_employment_ui <- function(id){
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
          outputId = ns("n_youth_with_employment_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 4,

        bs4Dash::box(
          title = "# of Youth by Employment Status",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("employed_pie_chart"),
            height = "350px"
          )
        )

      ),

      shiny::column(
        width = 4,

        bs4Dash::box(
          title = "# of Youth by Employment Type",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("employment_type_pie_chart"),
            height = "450px"
          )
        )

      ),

      shiny::column(
        width = 4,

        bs4Dash::box(
          title = "# of Youth by Reason Not Employed",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("not_employed_reason_pie_chart"),
            height = "450px"
          )
        )

      )

    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        bs4Dash::box(
          title = "Changes in Employed Status (Entry --> Exit)",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("employed_sankey_chart"),
            height = "350px"
          )
        )

      )
    ),

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

#' employment Server Functions
#'
#' @noRd
mod_employment_server <- function(id, employment_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the employment data
    employment_data_filtered <- shiny::reactive({

      employment_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    })

    # Total number of Youth in program(s) that exist in the `employment.csv`
    # file
    n_youth_with_employment_data <- shiny::reactive(

      employment_data_filtered() |>
        dplyr::filter(employed %in% c("Yes", "No")) |>
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
    output$n_youth_with_employment_data_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_employment_data(),
        subtitle = "Total # of Youth with Employment Data Available",
        icon = shiny::icon("home")
      )

    })

    # Create reactive data frame to data to be displayed in pie chart
    employed_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(employment_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- employment_data_filtered() |>
        dplyr::arrange(
          organization_id,
          personal_id,
          employed,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          employed
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          employed,
          .keep_all = TRUE
        ) |>
        dplyr::filter(employed %in% c("Yes", "No"))

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(employed) |>
        dplyr::arrange(employed)

    })

    # Create employment pie chart
    output$employed_pie_chart <- echarts4r::renderEcharts4r({

      employed_pie_chart_data() |>
        pie_chart(
          category = "employed",
          count = "n"
        )

    })

    # # Create reactive data frame to data to be displayed in pie chart
    # employment_type_pie_chart_data <- shiny::reactive({
    #
    #   shiny::validate(
    #     shiny::need(
    #       expr = nrow(employment_data_filtered()) >= 1L,
    #       message = "No data to display"
    #     )
    #   )
    #
    #   out <- employment_data_filtered() |>
    #     dplyr::filter(!is.na(employment_type)) |>
    #     dplyr::select(personal_id, information_date, employment_type) |>
    #     dplyr::arrange(personal_id, dplyr::desc(information_date)) |>
    #     dplyr::distinct(personal_id, .keep_all = TRUE) |>
    #     dplyr::select(employment_type)
    #
    #   shiny::validate(
    #     shiny::need(
    #       expr = nrow(out) >= 1L,
    #       message = "No data to display"
    #     )
    #   )
    #
    #   out |>
    #     dplyr::count(employment_type, sort = TRUE)
    #
    # })
    #
    # # Create employment type pie chart
    # output$employment_type_pie_chart <- echarts4r::renderEcharts4r({
    #
    #   employment_type_pie_chart_data() |>
    #     pie_chart(
    #       category = "employment_type",
    #       count = "n"
    #     )
    #
    # })
    #
    # # Create reactive data frame to data to be displayed in pie chart
    # not_employed_reason_pie_chart_data <- shiny::reactive({
    #
    #   shiny::validate(
    #     shiny::need(
    #       expr = nrow(employment_data_filtered()) >= 1L,
    #       message = "No data to display"
    #     )
    #   )
    #
    #   out <- employment_data_filtered() |>
    #     dplyr::filter(!is.na(not_employed_reason)) |>
    #     dplyr::select(personal_id, information_date, not_employed_reason) |>
    #     dplyr::arrange(personal_id, dplyr::desc(information_date)) |>
    #     dplyr::distinct(personal_id, .keep_all = TRUE) |>
    #     dplyr::select(not_employed_reason)
    #
    #   shiny::validate(
    #     shiny::need(
    #       expr = nrow(out) >= 1L,
    #       message = "No data to display"
    #     )
    #   )
    #
    #   out |>
    #     dplyr::count(not_employed_reason, sort = TRUE)
    #
    # })
    #
    # # Create "not employed reason" type pie chart
    # output$not_employed_reason_pie_chart <- echarts4r::renderEcharts4r({
    #
    #   not_employed_reason_pie_chart_data() |>
    #     pie_chart(
    #       category = "not_employed_reason",
    #       count = "n"
    #     )
    #
    # })

    # Create reactive data frame to data to be displayed in line chart
    employed_sankey_chart_data <- shiny::reactive({

      ids_exited <- employment_data_filtered() |>
        dplyr::filter(
          employed %in% c("Yes", "No")
        ) |>
        get_ids_for_sankey()

      shiny::validate(
        shiny::need(
          expr = nrow(ids_exited) >= 1L,
          message = "No data to display"
        )
      )

      employment_data_filtered() |>
        dplyr::filter(
          employed %in% c("Yes", "No")
        ) |>
        dplyr::inner_join(
          ids_exited,
          by = c("organization_id", "personal_id")
        ) |>
        prep_sankey_data(state_var = employed)

    })

    # Create disabilities trend line chart
    output$employed_sankey_chart <- echarts4r::renderEcharts4r({

      employed_sankey_chart_data() |>
        sankey_chart(
          entry_status = "Entry",
          exit_status = "Exit",
          count = "n"
        )

    })

    missingness_stats <- shiny::reactive({

      employment_data_filtered() |>
        dplyr::mutate(employed = ifelse(
          is.na(employed),
          "(Blank)",
          employed
        )) |>
        dplyr::filter(!employed %in% c("Yes", "No")) |>
        dplyr::count(employed, name = "Count") |>
        dplyr::rename(Response = employed)

    })

    output$missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        missingness_stats()
      )
    )

  })
}

## To be copied in the UI
# mod_employment_ui("employment_1")

## To be copied in the server
# mod_employment_server("employment_1")
