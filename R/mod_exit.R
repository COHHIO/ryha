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

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth")),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user", class = "fa-solid"),
        width = 6
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_exit_data")),
        subtitle = "Total # of Youth with Exit Data Available",
        icon = shiny::icon("door-open"),
        width = 6
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 12,

        bs4Dash::tabsetPanel(
          type = "pills",

          shiny::tabPanel(
            title = "Project Completion Status",

            shiny::fluidRow(

              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Youth by Project Completion Status",
                    content = link_section("R17 Project Completion Status")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("completion_chart"),
                    height = "100%"
                  )
                )

              )

            )

          ),

          shiny::tabPanel(
            title = "Safe & Appropriate Exit",

            shiny::fluidRow(

              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = with_popover(
                    text = "# of Youth by Safe & Appropriate Exit Response",
                    content = link_section("R19 Safe and Appropriate Exit")
                  ),
                  width = NULL,
                  height = DEFAULT_BOX_HEIGHT,
                  maximizable = TRUE,
                  echarts4r::echarts4rOutput(
                    outputId = ns("exit_heatmap"),
                    height = "100%"
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

#' exit Server Functions
#'
#' @noRd
mod_exit_server <- function(id, exit_data, clients_filtered, heads_of_household_and_adults){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the services data
    exit_data_filtered <- shiny::reactive({
      filter_data(exit_data, clients_filtered()) |>
        dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
    })

    # Total number of Youth in program(s) that exist in the `Exit.csv` file
    # (after filters applied)
    n_youth_with_exit_data <- shiny::reactive(

      exit_data_filtered() |>
        dplyr::filter(!is.na(project_completion_status)) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    # Render number of youth w/ services box value
    output$n_youth_with_exit_data <- shiny::renderText({
      n_youth_with_exit_data()
    })

    # Create employment pie chart
    output$completion_chart <- echarts4r::renderEcharts4r({
      exit_data_filtered() |>
        dplyr::count(project_completion_status, .drop = FALSE) |>
        bar_chart(
          x = "project_completion_status",
          y = "n"
        )
    })

    # Create employment pie chart
    output$exit_heatmap <- echarts4r::renderEcharts4r({

      exit_data_filtered() |>
        dplyr::count(destination_safe_client, destination_safe_worker, .drop = FALSE) |>
        echarts4r::e_charts(
          x = destination_safe_client,
          label = list(show = TRUE, fontSize = 16)
        ) |>
        echarts4r::e_heatmap(
          y = destination_safe_worker,
          z = n
        ) |>
        echarts4r::e_visual_map(
          serie = n,
          show = FALSE
        ) |>
        echarts4r::e_axis_labels(
          x = "Youth Response",
          y = "Worker Response"
        ) |>
        echarts4r::e_x_axis(
          nameLocation = "middle",
          nameGap = 60,
          nameTextStyle = list(fontSize = 14),
          axisLabel = list(
            interval = 0,
            width = 110,
            overflow = "break"
          )
        ) |>
        echarts4r::e_y_axis(
          nameLocation = "middle",
          nameGap = 160,
          nameTextStyle = list(fontSize = 14)
        ) |>
        echarts4r::e_tooltip(
          trigger = "item",
          formatter = htmlwidgets::JS("
            function(params){
                return('<strong>Youth Response: </strong>' + params.value[0] + '<br />' +
                        '<strong>Worker Response: </strong>' + params.value[1] + '<br />' +
                        params.marker + '# of Youth: ' + params.value[2]
                )
            }
          ")
        ) |>
        echarts4r::e_grid(containLabel = TRUE) |>
        echarts4r::e_show_loading()

    })

  })
}

## To be copied in the UI
# mod_exit_ui("exit_1")

## To be copied in the server
# mod_exit_server("exit_1")
