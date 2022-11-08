#' disabilities UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_disabilities_ui <- function(id){
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
          outputId = ns("n_youth_with_disabilities_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      shiny::column(
        width = 4,

        bs4Dash::box(
          title = "# of Disabled Youth by Disability Type",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("disabilities_pie_chart"),
            height = "600px"
          )
        )

      ),

      shiny::column(
        width = 8,

        bs4Dash::box(
          title = "Trend of Disability Types",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("disabilities_line_chart"),
            height = "600px"
          )
        )

      )

    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        bs4Dash::box(
          title = "# of Youth with 2 Disability Statuses",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(outputId = ns("disabilities_crosstab"))
        )

      )
    )

  )
}

#' disabilities Server Functions
#'
#' @noRd
mod_disabilities_server <- function(id, disabilities_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive({

      clients_filtered() |>
        nrow()

    })

    # Apply the filters to the disabilities data
    disabilities_data_filtered <- shiny::reactive({

      disabilities_data |>
        dplyr::filter(personal_id %in% clients_filtered()$personal_id) |>
        dplyr::left_join(
          clients_filtered() |>
            dplyr::select(personal_id, software_name, exit_date),
          by = c("personal_id", "software_name")
        )

    })

    # Total number of Youth in program(s) that exist in the `disabilities.csv`
    # file
    n_youth_with_disabilities_data <- shiny::reactive(

      disabilities_data_filtered() |>
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
    output$n_youth_with_disabilities_data_box <- bs4Dash::renderbs4ValueBox({

      bs4Dash::bs4ValueBox(
        value = n_youth_with_disabilities_data(),
        subtitle = "Total # of Youth with Disabilities Data Available",
        icon = shiny::icon("home")
      )

    })

    # Create reactive data frame to data to be displayed in pie chart
    pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(disabilities_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- disabilities_data_filtered() |>
        dplyr::arrange(personal_id, disability_type, dplyr::desc(information_date)) |>
        dplyr::select(personal_id, disability_type, disability_response) |>
        dplyr::distinct(personal_id, disability_type, .keep_all = TRUE) |>
        dplyr::filter(disability_response == "Yes")

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(disability_type) |>
        dplyr::arrange(disability_type)

    })

    # Create disabilities pie chart
    output$disabilities_pie_chart <- echarts4r::renderEcharts4r({

      pie_chart_data() |>
        pie_chart(
          category = "disability_type",
          count = "n"
        )

    })

    # Create reactive data frame to data to be displayed in line chart
    line_chart_data <- shiny::reactive({

      out <- disabilities_data_filtered() |>
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
        dplyr::filter(disability_response == "Yes")

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(disability_type, information_date) |>
        dplyr::group_by(disability_type)

    })

    # Create disabilities trend line chart
    output$disabilities_line_chart <- echarts4r::renderEcharts4r({

      line_chart_data() |>
        echarts4r::e_charts(information_date) |>
        echarts4r::e_line(n) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(top = "20%") |>
        echarts4r::e_show_loading()

    })

    # crosstab_data <- shiny::reactive({
    #
    #   filtered_dm()$disabilities |>
    #     dplyr::inner_join(
    #       filtered_dm()$submission |>
    #         dplyr::group_by(project_id) |>
    #         dplyr::filter(export_end_date == max(export_end_date)) |>
    #         dplyr::ungroup() |>
    #         dplyr::select(submission_id, project_id),
    #       by = "submission_id"
    #     ) |>
    #     dplyr::arrange(submission_id, personal_id, dplyr::desc(information_date)) |>
    #     dplyr::select(personal_id, disability_type, disability_response) |>
    #     dplyr::distinct(personal_id, disability_type, .keep_all = TRUE) |>
    #     dplyr::filter(disability_response == "Yes") |>
    #     dplyr::select(personal_id, disability_type) %>%
    #     dplyr::left_join(x = ., y = ., by = "personal_id") |>
    #     dplyr::group_by(disability_type.x, disability_type.y) |>
    #     dplyr::count() |>
    #     dplyr::ungroup() |>
    #     dplyr::mutate(n = ifelse(disability_type.x == disability_type.y, NA, n))
    #
    # })
    #
    # output$disabilities_crosstab <- echarts4r::renderEcharts4r({
    #
    #   crosstab_data() |>
    #   echarts4r::e_charts(
    #     x = disability_type.x,
    #     label = list(show = TRUE)   # show values inside cells
    #   ) |>
    #     echarts4r::e_heatmap(
    #       y = disability_type.y,
    #       z = n,
    #       pointSize = 5
    #     ) |>
    #     echarts4r::e_visual_map(
    #       serie = n,
    #       show = FALSE   # hide the interactive legend gradient"
    #     ) |>
    #     echarts4r::e_tooltip(
    #       trigger = "item",
    #       # Check out https://echarts4r.john-coene.com/articles/tooltip.html#javascript
    #       # for more context on how we created the custom tooltip
    #       formatter = htmlwidgets::JS("
    #         function(params){
    #           return('# of Youth With' +
    #           '<br /><em>' + params.value[0] + '</em> & <em>' + params.value[1] + '</em>' +
    #           '<br />' + params.marker + ' Count: <strong>' + params.value[2] + '</strong>')
    #         }
    #       ")
    #     ) |>
    #     echarts4r::e_grid(
    #       left = "20%",
    #       bottom = "20%"
    #     )
    #     # echarts4r::e_x_axis(
    #     #   axisLabel = list(rotate = 45)
    #     # )
    #
    # })

  })
}

## To be copied in the UI
# mod_disabilities_ui("disabilities_1")

## To be copied in the server
# mod_disabilities_server("disabilities_1")
