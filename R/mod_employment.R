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
            outputId = ns("employment_pie_chart"),
            height = "450px"
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
          title = "Trend of Employed Type",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("employed_line_chart"),
            height = "560px"
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
        dplyr::filter(personal_id %in% clients_filtered()$personal_id) |>
        dplyr::filter(employed %in% c("Yes", "No")) |>
        dplyr::left_join(
          clients_filtered() |>
            dplyr::select(personal_id, software_name, exit_date),
          by = c("personal_id", "software_name")
        )

    })

    # Total number of Youth in program(s) that exist in the `employment.csv`
    # file
    n_youth_with_employment_data <- shiny::reactive(

      employment_data_filtered() |>
        dplyr::distinct(personal_id, software_name) |>
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
    pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(employment_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- employment_data_filtered() |>
        dplyr::select(personal_id, information_date, employed) |>
        dplyr::arrange(personal_id, dplyr::desc(information_date)) |>
        dplyr::distinct(personal_id, .keep_all = TRUE) |>
        dplyr::select(employed)

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(employed, sort = TRUE)

    })

    # Create employment pie chart
    output$employment_pie_chart <- echarts4r::renderEcharts4r({

      pie_chart_data() |>
        pie_chart(
          category = "employed",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in pie chart
    employment_type_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(employment_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- employment_data_filtered() |>
        dplyr::filter(!is.na(employment_type)) |>
        dplyr::select(personal_id, information_date, employment_type) |>
        dplyr::arrange(personal_id, dplyr::desc(information_date)) |>
        dplyr::distinct(personal_id, .keep_all = TRUE) |>
        dplyr::select(employment_type)

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(employment_type, sort = TRUE)

    })

    # Create employment type pie chart
    output$employment_type_pie_chart <- echarts4r::renderEcharts4r({

      employment_type_pie_chart_data() |>
        pie_chart(
          category = "employment_type",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in pie chart
    not_employed_reason_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(employment_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- employment_data_filtered() |>
        dplyr::filter(!is.na(not_employed_reason)) |>
        dplyr::select(personal_id, information_date, not_employed_reason) |>
        dplyr::arrange(personal_id, dplyr::desc(information_date)) |>
        dplyr::distinct(personal_id, .keep_all = TRUE) |>
        dplyr::select(not_employed_reason)

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(not_employed_reason, sort = TRUE)

    })

    # Create "not employed reason" type pie chart
    output$not_employed_reason_pie_chart <- echarts4r::renderEcharts4r({

      not_employed_reason_pie_chart_data() |>
        pie_chart(
          category = "not_employed_reason",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in line chart
    employed_line_chart_data <- shiny::reactive({

      out <- employment_data_filtered() |>
        dplyr::filter(!is.na(exit_date)) |>
        dplyr::group_by(personal_id) |>
        dplyr::mutate(n = dplyr::n_distinct(information_date)) |>
        dplyr::filter(n >= 2L) |>
        dplyr::select(-n)

      shiny::validate(
        shiny::need(
          expr = any(
            nrow(out) >= 1L,
            nrow(dplyr::filter(out, !is.na(information_date))) >= 1L
          ),
          message = "No data to display"
        )
      )

      out <- out |>
        dplyr::filter(
          information_date %in% c(
            min(information_date, na.rm = TRUE),
            max(information_date, na.rm = TRUE)
          )
        )

      shiny::validate(
        shiny::need(
          expr = any(
            nrow(out) >= 1L,
            nrow(dplyr::filter(out, !is.na(information_date))) >= 1L
          ),
          message = "No data to display"
        )
      )

      out <- out |>
        dplyr::mutate(
          information_date = dplyr::if_else(
            information_date == min(information_date, na.rm = TRUE),
            "Entry",
            "Exit"
          ),
          information_date = factor(
            information_date,
            levels = c("Entry", "Exit")
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::count(employed, information_date, sort = TRUE) |>
        dplyr::group_by(employed)

    })

    # Create employed trend line chart
    output$employed_line_chart <- echarts4r::renderEcharts4r({

      employed_line_chart_data() |>
        echarts4r::e_charts(information_date) |>
        echarts4r::e_line(n) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(top = "20%") |>
        echarts4r::e_show_loading()

    })

  })
}

## To be copied in the UI
# mod_employment_ui("employment_1")

## To be copied in the server
# mod_employment_server("employment_1")
