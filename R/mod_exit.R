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
                    outputId = ns("completion_pie_chart"),
                    height = "100%"
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
                    outputId = ns("completion_missingness_stats_tbl")
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

            ),

            shiny::fluidRow(
              shiny::column(
                width = 12,

                bs4Dash::box(
                  title = "Data Quality Statistics",
                  width = NULL,
                  maximizable = TRUE,
                  reactable::reactableOutput(
                    outputId = ns("exit_missingness_stats_tbl")
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
          by = c("personal_id", "organization_id", "enrollment_id")
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

    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    # Render number of youth w/ services box value
    output$n_youth_with_exit_data <- shiny::renderText({
      n_youth_with_exit_data()
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

    # Create reactive data frame to data to be displayed in pie chart
    exit_heatmap_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(exit_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- exit_data_filtered() |>
        dplyr::filter(
          !is.na(destination_safe_client) & !is.na(destination_safe_worker)
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          destination_safe_client,
          destination_safe_worker,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          destination_safe_client,
          destination_safe_worker
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          destination_safe_client,
          destination_safe_worker,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(destination_safe_client, destination_safe_worker) |>
        dplyr::mutate(
          dplyr::across(
            .cols = -n,
            .fns = function(x) ifelse(is.na(x), "(Blank)", x)
          )
        ) |>
        dplyr::arrange(destination_safe_client, destination_safe_worker)

    })

    # Create employment pie chart
    output$exit_heatmap <- echarts4r::renderEcharts4r({

      exit_heatmap_data() |>
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
          nameGap = 40,
          nameTextStyle = list(fontSize = 14)
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

    # Compute "Project Completion Status" missingness stats
    completion_missingness_stats <- shiny::reactive(

      exit_data_filtered() |>
        dplyr::filter(is.na(project_completion_status)) |>
        dplyr::mutate(project_completion_status = "(Blank)") |>
        dplyr::count(project_completion_status, name = "Count") |>
        dplyr::rename(Response = project_completion_status)

    )

    # Create "Project Completion Status" missingness stats table
    output$completion_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        completion_missingness_stats()
      )
    )

    # Compute "Safe & Appropriate Exit" missingness stats
    exit_missingness_stats <- shiny::reactive({

      client <- exit_data_filtered() |>
        dplyr::mutate(destination_safe_client = ifelse(
          is.na(destination_safe_client),
          "(Blank)",
          destination_safe_client
        )) |>
        dplyr::filter(
          destination_safe_client %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(destination_safe_client, name = "Count (Youth)") |>
        dplyr::rename(Response = destination_safe_client)

      worker <- exit_data_filtered() |>
        dplyr::filter(is.na(destination_safe_worker)) |>
        dplyr::mutate(destination_safe_worker = "(Blank)") |>
        dplyr::count(destination_safe_worker, name = "Count (Worker)") |>
        dplyr::rename(Response = destination_safe_worker)

      client |>
        dplyr::full_join(worker, by = "Response")

    })

    # Create "Safe & Appropriate Exit" missingness stats table
    output$exit_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        exit_missingness_stats()
      )
    )

  })
}

## To be copied in the UI
# mod_exit_ui("exit_1")

## To be copied in the server
# mod_exit_server("exit_1")
