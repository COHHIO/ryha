#' services UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_services_ui <- function(id){
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
          outputId = ns("n_youth_with_services_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 3,

        shiny::dateRangeInput(
          inputId = ns("date_provided_filter"),
          label = "Date Provided",
          start = NULL,
          end = NULL,
          min = NULL,
          max = NULL
        )

      )

    ),

    shiny::fluidRow(

      shiny::column(
        width = 12,
        bs4Dash::box(
          title = "# of Youth by Service Type Provided",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("bar_chart"),
            height = "450px"
          )
        )
      )

    )

  )
}

#' services Server Functions
#'
#' @noRd
mod_services_server <- function(id, services_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Update the "Date Provided" date range filter
    shiny::updateDateRangeInput(
      session = session,
      inputId = "date_provided_filter",
      start = min(services_data$date_provided),
      end = max(services_data$date_provided),
      min = min(services_data$date_provided),
      max = max(services_data$date_provided)
    )

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the health data
    services_data_filtered <- shiny::reactive({

      services_data |>
        dplyr::filter(
          personal_id %in% clients_filtered()$personal_id,
          dplyr::between(
            date_provided,
            left = input$date_provided_filter[1],
            right = input$date_provided_filter[2]
          )
        ) |>
        dplyr::left_join(
          clients_filtered() |>
            dplyr::select(personal_id, software_name, exit_date),
          by = c("personal_id", "software_name")
        )

    })

    # Total number of Youth in program(s) that exist in the `services.csv`
    # file
    n_youth_with_services_data <- shiny::reactive(

      services_data_filtered() |>
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

    # Render number of youth w/ services box
    output$n_youth_with_services_data_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_services_data(),
        subtitle = "Total # of Youth with Services Data Available",
        icon = shiny::icon("home")
      )

    })

    # Create reactive data frame to data to be displayed in bar chart
    bar_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(services_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      services_data_filtered() |>
        dplyr::distinct(personal_id, type_provided) |>
        dplyr::count(type_provided) |>
        dplyr::arrange(n)

    })

    # Create services chart
    output$bar_chart <- echarts4r::renderEcharts4r({

      bar_chart_data() |>
        echarts4r::e_charts(x = type_provided) |>
        echarts4r::e_bar(
          serie = n,
          name = "# of Youth",
          legend = FALSE,
          label = list(
            formatter = '{@[0]}',
            show = TRUE,
            position = "right"
          )
        ) |>
        echarts4r::e_tooltip(trigger = "item") |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_grid(containLabel = TRUE) |>
        echarts4r::e_show_loading()

    })

  })
}

## To be copied in the UI
# mod_services_ui("services_1")

## To be copied in the server
# mod_services_server("services_1")
