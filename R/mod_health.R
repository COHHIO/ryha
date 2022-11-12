#' health UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_health_ui <- function(id){
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
          outputId = ns("n_youth_with_health_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 4,
        bs4Dash::box(
          title = "# of Youth by General Health Status",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("general_health_pie_chart"),
            height = "450px"
          )
        )
      ),

      shiny::column(
        width = 4,
        bs4Dash::box(
          title = "# of Youth by Dental Health Status",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("dental_health_pie_chart"),
            height = "450px"
          )
        )
      ),

      shiny::column(
        width = 4,
        bs4Dash::box(
          title = "# of Youth by Mental Health Status",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("mental_health_pie_chart"),
            height = "450px"
          )
        )
      )

    ),

    shiny::fluidRow(

      shiny::column(
        width = 12,

        bs4Dash::box(
          title = "Trend of General Health Status",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("health_line_chart"),
            height = "560px"
          )
        )

      )

    )

  )
}

#' health Server Functions
#'
#' @noRd
mod_health_server <- function(id, health_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the health data
    health_data_filtered <- shiny::reactive({

      health_data |>
        dplyr::filter(personal_id %in% clients_filtered()$personal_id) |>
        dplyr::left_join(
          clients_filtered() |>
            dplyr::select(personal_id, software_name, exit_date),
          by = c("personal_id", "software_name")
        ) |>
        dplyr::mutate(
          dplyr::across(
            .cols = general_health_status:mental_health_status,
            .fns = function(x) factor(x, levels = HealthStatusCodes$Description)
          )
        )

    })

    # Total number of Youth in program(s) that exist in the `health.csv`
    # file
    n_youth_with_health_data <- shiny::reactive(

      health_data_filtered() |>
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
    output$n_youth_with_health_data_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_health_data(),
        subtitle = "Total # of Youth with Health Data Available",
        icon = shiny::icon("home")
      )

    })

    # Create reactive data frame to data to be displayed in pie chart
    general_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(health_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- health_data_filtered() |>
        dplyr::select(personal_id, information_date, general_health_status) |>
        dplyr::arrange(personal_id, dplyr::desc(information_date)) |>
        dplyr::distinct(personal_id, .keep_all = TRUE) |>
        dplyr::select(general_health_status)

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(general_health_status, sort = TRUE)

    })

    # Create health pie chart
    output$general_health_pie_chart <- echarts4r::renderEcharts4r({

      general_pie_chart_data() |>
        pie_chart(
          category = "general_health_status",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in pie chart
    dental_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(health_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- health_data_filtered() |>
        dplyr::select(personal_id, information_date, dental_health_status) |>
        dplyr::arrange(personal_id, dplyr::desc(information_date)) |>
        dplyr::distinct(personal_id, .keep_all = TRUE) |>
        dplyr::select(dental_health_status)

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(dental_health_status, sort = TRUE)

    })

    # Create health pie chart
    output$dental_health_pie_chart <- echarts4r::renderEcharts4r({

      dental_pie_chart_data() |>
        pie_chart(
          category = "dental_health_status",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in pie chart
    mental_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(health_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- health_data_filtered() |>
        dplyr::select(personal_id, information_date, mental_health_status) |>
        dplyr::arrange(personal_id, dplyr::desc(information_date)) |>
        dplyr::distinct(personal_id, .keep_all = TRUE) |>
        dplyr::select(mental_health_status)

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(mental_health_status, sort = TRUE)

    })

    # Create health pie chart
    output$mental_health_pie_chart <- echarts4r::renderEcharts4r({

      mental_pie_chart_data() |>
        pie_chart(
          category = "mental_health_status",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in line chart
    health_line_chart_data <- shiny::reactive({

      out <- health_data_filtered() |>
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
        dplyr::count(general_health_status, information_date, sort = TRUE) |>
        dplyr::group_by(general_health_status)

    })

    # Create health trend line chart
    output$health_line_chart <- echarts4r::renderEcharts4r({

      health_line_chart_data() |>
        echarts4r::e_charts(information_date) |>
        echarts4r::e_line(n) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(top = "20%") |>
        echarts4r::e_show_loading()

    })

  })
}

## To be copied in the UI
# mod_health_ui("health_1")
## To be copied in the server
# mod_health_server("health_1")
