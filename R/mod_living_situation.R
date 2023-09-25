#' living_situation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_living_situation_ui <- function(id){
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
        # Number of youth with living situation data available (post filters)
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_living_situation_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = "# of Youth by Living Situation (at Entry)",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("living_situation_pie_chart"),
            height = "350px"
          )
        )

      ),

      shiny::column(
        width = 6,

        bs4Dash::box(
          title = "# of Youth by Destination (at Exit)",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("destination_pie_chart"),
            height = "350px"
          )
        )

      )

    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        bs4Dash::box(
          title = "Changes in General Living Situation (Entry --> Exit)",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("sankey_chart"),
            height = "350px"
          )
        )

      )
    ),

    bs4Dash::tabsetPanel(
      type = "pills",

      ## Benefits Tab Panel ----
      shiny::tabPanel(
        title = "Living Situation (Entry)",

        shiny::fluidRow(
          shiny::column(
            width = 12,

            bs4Dash::box(
              title = "Data Quality Statistics - Living Situation (Entry)",
              width = NULL,
              maximizable = TRUE,
              reactable::reactableOutput(
                outputId = ns("living_situation_missingness_stats_tbl")
              )
            )

          )
        )

      ),

      shiny::tabPanel(
        title = "Destination (Exit)",

        shiny::fluidRow(
          shiny::column(
            width = 12,

            bs4Dash::box(
              title = "Data Quality Statistics - Destination (Exit)",
              width = NULL,
              maximizable = TRUE,
              reactable::reactableOutput(
                outputId = ns("destination_missingness_stats_tbl")
              )
            )

          )
        )

      )

    )

  )
}

#' living_situation Server Functions
#'
#' @noRd
mod_living_situation_server <- function(id, project_data, enrollment_data, exit_data, clients_filtered, rctv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the enrollment data
    living_data_filtered <- shiny::reactive({

      shiny::req(rctv$selected_projects)

      project_ids <- project_data |>
        dplyr::filter(project_name %in% rctv$selected_projects) |>
        dplyr::pull(project_id)

      enrollment_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        ) |>
        dplyr::filter(project_id %in% project_ids) |>
        dplyr::left_join(
          exit_data |>
            dplyr::select(
              enrollment_id,
              organization_id,
              destination
            ),
          by = c("enrollment_id", "organization_id")
        )

    })

    # Total number of Youth in program(s) that exist in the `employment.csv`
    # file
    n_youth_with_living_data <- shiny::reactive(

      living_data_filtered() |>
        dplyr::filter(
          !is.na(living_situation),
          !living_situation %in% c(
            "No exit interview completed",
            "Worker unable to determine",
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected"
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
        icon = shiny::icon("user", class = "fa-solid")
      )

    })

    # Render number of youth with living situation data box
    output$n_youth_with_living_situation_data_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_living_data(),
        subtitle = "Total # of Youth with Living Situation Data Available",
        icon = shiny::icon("user", class = "fa-solid")
      )

    })

    # Living Situation Pie Chart ----

    # Create reactive data frame to data to be displayed in pie chart
    living_situation_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(living_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- living_data_filtered() |>
        dplyr::arrange(
          organization_id,
          personal_id,
          living_situation,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          living_situation
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          living_situation,
          .keep_all = TRUE
        ) |>
        dplyr::inner_join(
          LivingCodes |>
            dplyr::select(
              description = Description,
              category = ExitCategory
            ),
          by = c("living_situation" = "description")
        ) |>
        dplyr::filter(category != "Not enough data")


      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(category) |>
        dplyr::arrange(category)

    })

    # Create living situation pie chart
    output$living_situation_pie_chart <- echarts4r::renderEcharts4r({

      living_situation_pie_chart_data() |>
        pie_chart(
          category = "category",
          count = "n"
        )

    })

    # Destination Pie Chart ----

    # Create reactive data frame to data to be displayed in pie chart
    destination_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(living_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- living_data_filtered() |>
        dplyr::arrange(
          organization_id,
          personal_id,
          destination,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          destination
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          destination,
          .keep_all = TRUE
        ) |>
        dplyr::inner_join(
          LivingCodes |>
            dplyr::select(
              description = Description,
              category = ExitCategory
            ),
          by = c("destination" = "description")
        ) |>
        dplyr::filter(category != "Not enough data")

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(category) |>
        dplyr::arrange(category)

    })

    # Create employment type pie chart
    output$destination_pie_chart <- echarts4r::renderEcharts4r({

      destination_pie_chart_data() |>
        pie_chart(
          category = "category",
          count = "n"
        )

    })


    # Sankey Chart ----

    # Create reactive data frame to data to be displayed in line chart
    sankey_chart_data <- shiny::reactive({

      sankey_data <- living_data_filtered() |>
        dplyr::select(
          organization_id,
          personal_id,
          living_situation,
          destination
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          .keep_all = TRUE
        ) |>
        dplyr::mutate(
          living_situation = LivingCodes$ExitCategory[match(x = living_situation, table = LivingCodes$Description)],
          destination = LivingCodes$ExitCategory[match(x = destination, table = LivingCodes$Description)],
        ) |>
        dplyr::filter(
          living_situation != "Not enough data",
          destination != "Not enough data"
        ) |>
        dplyr::count(living_situation, destination) |>
        dplyr::arrange(living_situation, destination)

      shiny::validate(
        shiny::need(
          expr = nrow(sankey_data) >= 1L,
          message = "No data to display"
        )
      )

      sankey_data |>
        dplyr::mutate(
          living_situation = paste0(living_situation, " (Entry)"),
          destination = paste0(destination, " (Exit)")
        )

    })

    # Create sankey chart
    output$sankey_chart <- echarts4r::renderEcharts4r({

      sankey_chart_data() |>
        sankey_chart(
          entry_status = "living_situation",
          exit_status = "destination",
          count = "n"
        )

    })

    # Data Quality Stats Tables ----

    ## Living Situation Data Quality ----
    living_situation_missingness_stats <- shiny::reactive({

      living_data_filtered() |>
        dplyr::mutate(living_situation = ifelse(
          is.na(living_situation),
          "(Blank)",
          living_situation
        )) |>
        dplyr::filter(
          living_situation %in% c(
            "No exit interview completed",
            "Worker unable to determine",
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(living_situation, name = "Count") |>
        dplyr::rename(Response = living_situation)

    })

    output$living_situation_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        living_situation_missingness_stats()
      )
    )

    ## Destination Data Quality ----
    destination_missingness_stats <- shiny::reactive({

      living_data_filtered() |>
        dplyr::mutate(destination = ifelse(
          is.na(destination),
          "(Blank)",
          destination
        )) |>
        dplyr::filter(
          destination %in% c(
            "No exit interview completed",
            "Worker unable to determine",
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(destination, name = "Count") |>
        dplyr::rename(Response = destination)

    })

    output$destination_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        destination_missingness_stats()
      )
    )

  })
}

## To be copied in the UI
# mod_living_situation_ui("living_situation_1")

## To be copied in the server
# mod_living_situation_server("living_situation_1")
