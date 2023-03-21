#' exit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_exit_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidRow(

      shiny::column(
        width = 6,
        # Number of youth (post filters)
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_box"),
          width = "100%"
        )
      ),

      shiny::column(
        width = 6,
        # Number of youth with exit data (post filters)
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_exit_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = "# of Youth by Project Completion Status",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("completion_pie_chart"),
            height = "350px"
          )
        )

      )

    )

  )
}

#' exit Server Functions
#'
#' @noRd
mod_exit_server <- function(id, exit_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the services data
    exit_data_filtered <- shiny::reactive(

      exit_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    )

    # Total number of Youth in program(s) that exist in the `Exit.csv` file
    # (after filters applied)
    n_youth_with_exit_data <- shiny::reactive(

      exit_data_filtered() |>
        dplyr::filter(!is.na(project_completion_status)) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Render number of youth box
    output$n_youth_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth(),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user")
      )

    })

    # Render number of youth w/ services box
    output$n_youth_with_exit_data_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_exit_data(),
        subtitle = "Total # of Youth with Services Data Available",
        icon = shiny::icon("home")
      )

    })

    # Create reactive data frame to data to be displayed in pie chart
    completion_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(exit_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- exit_data_filtered() |>
        dplyr::filter(!is.na(project_completion_status)) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          project_completion_status,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          project_completion_status
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          project_completion_status,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(project_completion_status) |>
        dplyr::arrange(project_completion_status)

    })

    # Create employment pie chart
    output$completion_pie_chart <- echarts4r::renderEcharts4r({

      completion_pie_chart_data() |>
        pie_chart(
          category = "project_completion_status",
          count = "n"
        )

    })

  })
}

## To be copied in the UI
# mod_exit_ui("exit_1")

## To be copied in the server
# mod_exit_server("exit_1")
