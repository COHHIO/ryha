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
            content = shiny::tagList(
              shiny::p("Each bar represents the percentage of youth who self-identify with a given gender category."),
              shiny::p("Since individuals can select multiple categories, the total percentage may exceed 100%."),
              shiny::p(link_section("3.06 Gender"))
            )
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("gender_chart"),
            height = "100%"
          )
        )

      ),

      shiny::column(
        width = 6,

        # Sexual Orientation ----

        bs4Dash::box(
          title = with_popover(
            text = "# of Head of Household and Adults by Sexual Orientation",
            content = link_section("R3 Sexual Orientation")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("sexual_orientation_chart"),
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
            text = "# of Adults by Veteran Status",
            content = link_section("3.07 Veteran Status")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("veteran_chart"),
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
            content = shiny::tagList(
              shiny::p("Each bar represents the percentage of youth who self-identify with a given racial and/or ethnic category."),
              shiny::p("Since individuals can select multiple categories, the total percentage may exceed 100%."),
              shiny::p(link_section("3.04 Race and Ethnicity"))
            ) 
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
            text = "# of Youth by Age Group",
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
            text = "# of Head of Household and Adults by Former Ward Child Welfare Response",
            content = link_section("R11 Formerly a Ward of Child Welfare/Foster Care Agency")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("welfare_chart"),
            height = "100%"
          )
        )

      ),

      shiny::column(
        width = 6,

        # Juvenile ----

        bs4Dash::box(
          title = with_popover(
            text = "# of Head of Household and Adults by Former Ward Juvenile Justice Response",
            content = link_section("R12 Formerly a Ward of Juvenile Justice System")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("juvenile_chart"),
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
mod_overview_server <- function(id, client_data, enrollment_data, gender_data, ethnicity_data, clients_filtered, heads_of_household_and_adults){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Gender ----
    output$gender_chart <- echarts4r::renderEcharts4r({
      gender_data |> 
        filter_data(clients_filtered(), at = "youth") |>
        dplyr::count(gender, .drop = FALSE) |>
        bar_chart(
          x = "gender",
          y = "n",
          pct_denominator = nrow(clients_filtered())
        )
    })

    # Filter enrollment data
    enrollment_data_filtered <- shiny::reactive({
      filter_data(enrollment_data, clients_filtered()) |>
        dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
    })

    # Sexual Orientation ----
    output$sexual_orientation_chart <- echarts4r::renderEcharts4r({
      enrollment_data_filtered() |>
        dplyr::count(sexual_orientation, .drop = FALSE) |>
        bar_chart(
          x = "sexual_orientation",
          y = "n"
        )
    })

    # Filter client data
    client_data_filtered <- shiny::reactive({
      filter_data(client_data, clients_filtered(), at = "youth")
    })

    # Veteran ----
    output$veteran_chart <- echarts4r::renderEcharts4r({
      client_data_filtered() |>
        dplyr::filter(age >= 18) |>
        dplyr::count(veteran_status, .drop = FALSE) |>
        bar_chart(
          x = "veteran_status",
          y = "n"
        )
    })

    # Age ----
    output$age_bar_chart <- echarts4r::renderEcharts4r({
      client_data_filtered() |>
        dplyr::count(age_grouped, .drop = FALSE) |>
        bar_chart(
          x = "age_grouped",
          y = "n"
        ) |>
        echarts4r::e_axis_labels(x = "# of Youth", y = "Age")
    })

    # Ethnicity ----
    output$ethnicity_bar_chart <- echarts4r::renderEcharts4r({
      ethnicity_data |> 
        filter_data(clients_filtered(), at = "youth") |>
        dplyr::count(ethnicity, .drop = FALSE) |>
        bar_chart(
          x = "ethnicity",
          y = "n",
          pct_denominator = nrow(clients_filtered())
        )
    })

    # Welfare ----
    output$welfare_chart <- echarts4r::renderEcharts4r(
      enrollment_data_filtered() |>
        dplyr::count(former_ward_child_welfare, .drop = FALSE) |>
        bar_chart(
          x = "former_ward_child_welfare",
          y = "n"
        )
    )

    # Juvenile ----
    output$juvenile_chart <- echarts4r::renderEcharts4r(
      enrollment_data_filtered() |>
        dplyr::count(former_ward_juvenile_justice, .drop = FALSE) |>
        bar_chart(
          x = "former_ward_juvenile_justice",
          y = "n"
        )
    )

  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
