#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidRow(

      shiny::column(
        width = 6,

        # Gender ----

        bs4Dash::box(
          title = with_popover(
            text = "# of Youth by Gender",
            content = link_section("3.06 Gender")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("gender_pie_chart"),
            height = "100%"
          )
        )

      ),

      shiny::column(
        width = 6,

        # Sexual Orientation ----

        bs4Dash::box(
          title = with_popover(
            text = "# of Youth by Sexual Orientation",
            content = link_section("R3 Sexual Orientation")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("sexual_orientation_pie_chart"),
            height = "100%"
          )
        )

      )

    ),

    shiny::fluidRow(

      shiny::column(
        width = 6,

        # Veteran Status ----

        bs4Dash::box(
          title = with_popover(
            text = "# of Youth by Veteran Status",
            content = link_section("3.07 Veteran Status")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("veteran_pie_chart"),
            height = "100%"
          )
        )

      ),

      shiny::column(
        width = 6,

        # Ethnicity ----

        bs4Dash::box(
          title = with_popover(
            text = "# of Youth by Race & Ethnicity",
            content = link_section("3.04 Race and Ethnicity")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("ethnicity_bar_chart"),
            height = "100%"
          )
        )

      )

    ),

    shiny::fluidRow(

      shiny::column(
        width = 12,

        # Age ----

        bs4Dash::box(
          title = with_popover(
            text = "# of Youth by Age",
            content = link_section("3.03 Date of Birth")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("age_bar_chart"),
            height = "100%"
          )
        )

      )

    ),


    shiny::fluidRow(

      shiny::column(
        width = 6,

        # Welfare ----

        bs4Dash::box(
          title = with_popover(
            text = "# of Youth by Former Ward Child Welfare Response",
            content = link_section("R11 Formerly a Ward of Child Welfare/Foster Care Agency")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("welfare_pie_chart"),
            height = "100%"
          )
        )

      ),

      shiny::column(
        width = 6,

        # Juvenile ----

        bs4Dash::box(
          title = with_popover(
            text = "# of Youth by Former Ward Juvenile Justice Response",
            content = link_section("R12 Formerly a Ward of Juvenile Justice System")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("juvenile_pie_chart"),
            height = "100%"
          )
        )

      )

    )

  )
}

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id, client_data, enrollment_data, gender_data,
                                ethnicity_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Gender ----

    # Apply the filters to the gender data
    gender_data_filtered <- shiny::reactive({

      gender_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    })

    # Create reactive data frame to data to be displayed in pie chart
    gender_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(gender_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      gender_data_filtered() |>
        dplyr::count(gender) |>
        dplyr::arrange(gender)

    })

    output$gender_pie_chart <- echarts4r::renderEcharts4r({

      gender_pie_chart_data() |>
        pie_chart(
          category = "gender",
          count = "n"
        )

    })

    # Apply the filters to the client data
    enrollment_data_filtered <- shiny::reactive({

      enrollment_data |>
        dplyr::select(
          personal_id,
          organization_id,
          sexual_orientation,
          former_ward_child_welfare,
          former_ward_juvenile_justice
        ) |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    })

    # Sexual Orientation ----

    # Create reactive data frame to data to be displayed in pie chart
    sexual_orientation_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(enrollment_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      enrollment_data_filtered() |>
        dplyr::mutate(sexual_orientation = ifelse(
          is.na(sexual_orientation),
          "Missing Data",
          sexual_orientation
        )) |>
        dplyr::count(sexual_orientation) |>
        dplyr::arrange(sexual_orientation)

    })

    output$sexual_orientation_pie_chart <- echarts4r::renderEcharts4r({

      sexual_orientation_pie_chart_data() |>
        pie_chart(
          category = "sexual_orientation",
          count = "n"
        )

    })

    # Apply the filters to the client data
    client_data_filtered <- shiny::reactive({

      client_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    })

    # Veteran ----

    # Create reactive data frame to data to be displayed in pie chart
    veteran_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(client_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      client_data_filtered() |>
        dplyr::mutate(veteran_status = ifelse(
          is.na(veteran_status),
          "Missing Data",
          veteran_status
        )) |>
        dplyr::count(veteran_status) |>
        dplyr::arrange(veteran_status)

    })

    output$veteran_pie_chart <- echarts4r::renderEcharts4r({

      veteran_pie_chart_data() |>
        pie_chart(
          category = "veteran_status",
          count = "n"
        )

    })

    # Age ----

    # Create reactive data frame to data to be displayed in pie chart
    age_bar_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(client_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      client_data_filtered() |>
        dplyr::filter(!is.na(age)) |>
        dplyr::count(age) |>
        dplyr::arrange(age) |>
        dplyr::mutate(age = as.factor(age))

    })

    output$age_bar_chart <- echarts4r::renderEcharts4r({

      age_bar_chart_data() |>
        bar_chart(
          x = "age",
          y = "n",
          axis_flip = FALSE
        ) |>
        echarts4r::e_axis_labels(x = "Age", y = "# of Youth")

    })

    # Ethnicity ----

    # Apply the filters to the ethnicity data
    ethnicity_data_filtered <- shiny::reactive({

      ethnicity_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    })

    # Create reactive data frame to data to be displayed in pie chart
    ethnicity_bar_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(ethnicity_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      ethnicity_data_filtered() |>
        dplyr::filter(!is.na(ethnicity)) |>
        dplyr::count(ethnicity) |>
        dplyr::arrange(n)

    })

    output$ethnicity_bar_chart <- echarts4r::renderEcharts4r({

      ethnicity_bar_chart_data() |>
        bar_chart(
          x = "ethnicity",
          y = "n"
        )

    })

    # Welfare ----

    # Create reactive data frame to data to be displayed in pie chart
    welfare_pie_chart_data <- shiny::reactive({

      shiny::req(enrollment_data_filtered())

      out <- enrollment_data_filtered() |>
        dplyr::filter(!is.na(former_ward_child_welfare))

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(former_ward_child_welfare) |>
        dplyr::arrange(former_ward_child_welfare)

    })

    output$welfare_pie_chart <- echarts4r::renderEcharts4r(

      welfare_pie_chart_data() |>
        pie_chart(
          category = "former_ward_child_welfare",
          count = "n"
        )

    )

    # Juvenile ----

    # Create reactive data frame to data to be displayed in pie chart
    juvenile_pie_chart_data <- shiny::reactive({

      shiny::req(enrollment_data_filtered())

      out <- enrollment_data_filtered() |>
        dplyr::filter(!is.na(former_ward_juvenile_justice))

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(former_ward_juvenile_justice) |>
        dplyr::arrange(former_ward_juvenile_justice)

    })

    output$juvenile_pie_chart <- echarts4r::renderEcharts4r(

      juvenile_pie_chart_data() |>
        pie_chart(
          category = "former_ward_juvenile_justice",
          count = "n"
        )

    )

  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
