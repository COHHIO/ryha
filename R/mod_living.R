#' living UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_living_ui <- function(id){
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
          outputId = ns("n_youth_with_living_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 4,

        bs4Dash::box(
          title = "# of Youth by Current Living Situation",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("living_pie_chart"),
            height = "600px"
          )
        )

      ),

      shiny::column(
        width = 8,

        bs4Dash::box(
          title = "Trend of Current Living Situation",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("living_line_chart"),
            height = "600px"
          )
        )

      )

    )

  )
}

#' living Server Functions
#'
#' @noRd
mod_living_server <- function(id, living_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the living data
    living_data_filtered <- shiny::reactive({

      living_data |>
        dplyr::filter(personal_id %in% clients_filtered()$personal_id) |>
        dplyr::left_join(
          clients_filtered() |>
            dplyr::select(personal_id, software_name, exit_date),
          by = c("personal_id", "software_name")
        )

    })

    # Total number of Youth in program(s) that exist in the
    # `current_living_situation.csv` file
    n_youth_with_living_data <- shiny::reactive(

      living_data_filtered() |>
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
    output$n_youth_with_living_data_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_living_data(),
        subtitle = "Total # of Youth with Current Living Situation Data Available",
        icon = shiny::icon("home")
      )

    })

    # Create reactive data frame to data to be displayed in pie chart
    pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(living_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- living_data_filtered() |>
        dplyr::select(personal_id, information_date, current_living_situation) |>
        dplyr::arrange(personal_id, dplyr::desc(information_date)) |>
        dplyr::distinct(personal_id, .keep_all = TRUE) |>
        dplyr::select(current_living_situation)

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(current_living_situation, sort = TRUE)

    })

    # Create current living situation pie chart
    output$living_pie_chart <- echarts4r::renderEcharts4r({

      pie_chart_data() |>
        pie_chart(
          category = "current_living_situation",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in line chart
    line_chart_data <- shiny::reactive({

      out <- living_data_filtered() |>
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
        dplyr::count(current_living_situation, information_date, sort = TRUE) |>
        dplyr::group_by(current_living_situation)

    })

    # Create current living situation trend line chart
    output$living_line_chart <- echarts4r::renderEcharts4r({

      line_chart_data() |>
        echarts4r::e_charts(information_date) |>
        echarts4r::e_line(n) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(top = "20%") |>
        echarts4r::e_show_loading()

    })

  })
}

## To be copied in the UI
# mod_living_ui("living_1")

## To be copied in the server
# mod_living_server("living_1")
