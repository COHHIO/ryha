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

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth")),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user", class = "fa-solid"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_services")),
        subtitle = "Total # of Youth with Services Data Available",
        icon = shiny::icon("hands-helping"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_referral")),
        subtitle = "Total # of Youth with Referral Data Available",
        icon = shiny::icon("hands-helping"),
        width = 4
      ),

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
          title = with_popover(
            text = "# of Youth by Service Type Provided",
            content = link_section("R14 RHY Service Connections")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("services_bar_chart"),
            height = "100%"
          )
        )
      )

    ),

    shiny::fluidRow(

      shiny::column(
        width = 12,
        bs4Dash::box(
          title = with_popover(
            text = "# of Youth by Referral Source",
            content = link_section("R1 Referral Source")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("referral_bar_chart"),
            height = "100%"
          )
        )
      )

    )

  )
}

#' services Server Functions
#'
#' @noRd
mod_services_server <- function(id, services_data, referral_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observe({

      shiny::req(
        nrow(services_data) >= 1L,
        any(!is.na(services_data$date_provided))
      )

      # Update the "Date Provided" date range filter
      shiny::updateDateRangeInput(
        session = session,
        inputId = "date_provided_filter",
        start = min(services_data$date_provided),
        end = max(services_data$date_provided),
        min = min(services_data$date_provided),
        max = max(services_data$date_provided)
      )

    })

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the services data
    services_data_filtered <- shiny::reactive(

      services_data |>
        dplyr::filter(
          dplyr::between(
            date_provided,
            left = input$date_provided_filter[1],
            right = input$date_provided_filter[2]
          )
        ) |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id", "enrollment_id")
        )

    )

    # Total number of Youth in program(s) that exist in the `services.csv`
    # file
    n_youth_with_services_data <- shiny::reactive(

      services_data_filtered() |>
        dplyr::filter(!is.na(type_provided)) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Apply the filters to the referral data
    referral_data_filtered <- shiny::reactive({

      shiny::req(services_data_filtered())

      referral_data |>
        dplyr::inner_join(
          services_data_filtered() |>
            dplyr::select(personal_id, organization_id),
          by = c("personal_id", "organization_id")
        ) |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id", "enrollment_id")
        )

    })

    # Total number of Youth with referral data available
    n_youth_with_referral_data <- shiny::reactive(

      referral_data_filtered() |>
        dplyr::filter(!is.na(referral_source)) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    # Render number of youth w/ services box value
    output$n_youth_with_services <- shiny::renderText({
      n_youth_with_services_data()
    })

    # Render number of youth w/ referral box value
    output$n_youth_with_referral <- shiny::renderText({
      n_youth_with_referral_data()
    })

    # Create reactive data frame to data to be displayed in bar chart
    services_bar_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(services_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      services_data_filtered() |>
        dplyr::filter(!is.na(type_provided)) |>
        dplyr::distinct(personal_id, organization_id, type_provided) |>
        dplyr::count(type_provided) |>
        dplyr::arrange(n)

    })

    # Create services chart
    output$services_bar_chart <- echarts4r::renderEcharts4r({

      services_bar_chart_data() |>
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

    # Create reactive data frame to data to be displayed in bar chart
    referral_bar_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(referral_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      referral_data_filtered() |>
        dplyr::filter(!is.na(referral_source)) |>
        dplyr::distinct(personal_id, organization_id, referral_source) |>
        dplyr::count(referral_source) |>
        dplyr::arrange(n)

    })

    # Create referral source chart
    output$referral_bar_chart <- echarts4r::renderEcharts4r({

      referral_bar_chart_data() |>
        echarts4r::e_charts(x = referral_source) |>
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
