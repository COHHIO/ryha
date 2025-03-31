#' parenting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parenting_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Info Boxes ----
    shiny::fluidRow(

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth")),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user", class = "fa-solid"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_pregnancy_data")),
        subtitle = "Total # of Youth with Pregnancy Data Available",
        icon = shiny::icon("baby-carriage"),
        width = 4
      ),

      bs4Dash::bs4ValueBox(
        value = shiny::textOutput(outputId = ns("n_youth_with_parenting_data")),
        subtitle = "Total # of Youth with Parenting Data Available",
        icon = shiny::icon("baby-carriage"),
        width = 4
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      # Pregnancy Chart ----
      shiny::column(
        width = 6,

        bs4Dash::box(
          title = with_tooltip(
            text = "# of Head of Household and/or Adults by Pregnancy Status",
            content = link_section("R10 Pregnancy Status")
          ),
          width = NULL,
          height = DEFAULT_BOX_HEIGHT,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("pregnancy_chart"),
            height = "100%"
          )
        )

      ),

      shiny::column(
        width = 6,

        # Parenting Table ----
        bs4Dash::box(
          title = with_tooltip(
            text = "# of Youth Parenting",
            content = shiny::HTML("A youth is defined as <strong>Parenting</strong> if there is at least one youth enrolled as the head of household's child")
          ),
          width = NULL,
          maximizable = TRUE,
          reactable::reactableOutput(
            outputId = ns("parenting_tbl")
          )
        )

      )

    )

  )
}

#' parenting Server Functions
#'
#' @noRd
mod_parenting_server <- function(id, health_data, enrollment_data, clients_filtered, heads_of_household_and_adults){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Info Boxes ----

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive(

      clients_filtered() |>
        nrow()

    )

    # Filter health data
    health_data_filtered <- shiny::reactive(
      filter_data(health_data, clients_filtered()) |>
        dplyr::semi_join(heads_of_household_and_adults, by = c("enrollment_id", "personal_id", "organization_id"))
    )

    most_recent_data_per_enrollment <- shiny::reactive({
      health_data_filtered() |> 
        dplyr::filter(data_collection_stage %in% c("Project start", "Project exit")) |>
        filter_most_recent_data_per_enrollment()
    })

    # Filter enrollment data
    enrollment_data_filtered <- shiny::reactive(
      enrollment_data |>
        filter_data(clients_filtered()) 
    )

    # Total number of Youth in program(s) provided a "Yes" or "No" response to
    # `pregnancy_status` in 'Health.csv"
    n_youth_with_pregnancy_data <- shiny::reactive(

      health_data_filtered() |>
        dplyr::filter(
          pregnancy_status %in% c("Yes", "No")
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )

    # Total number of Youth in program(s) provided a valid response to
    # `relationship_to_hoh` in "Enrollment.csv"
    n_youth_with_parenting_data <- shiny::reactive(

      enrollment_data_filtered() |>
        dplyr::filter(
          !is.na(relationship_to_ho_h)
        ) |>
        dplyr::distinct(personal_id, organization_id) |>
        nrow()

    )


    # Render number of clients box value
    output$n_youth <- shiny::renderText({
      n_youth()
    })

    # Render "number youth with pregnancy data available" box value
    output$n_youth_with_pregnancy_data <- shiny::renderText({
      n_youth_with_pregnancy_data()
    })

    # Render "number youth with parenting data available" box value
    output$n_youth_with_parenting_data <- shiny::renderText({
      n_youth_with_parenting_data()
    })

    # Pregnancy Status ----
    output$pregnancy_chart <- echarts4r::renderEcharts4r({
      most_recent_data_per_enrollment() |> 
        dplyr::count(pregnancy_status, .drop = FALSE) |> 
        bar_chart(
          x = "pregnancy_status",
          y = "n"
        )
    })

    ## Data Quality Statistics ----

    # Parenting ----

    ## Table ----

    # Create reactive data frame to capture number of youth who are parenting
    parenting_data <- shiny::reactive({

      hhs_with_children <- enrollment_data_filtered() |>
        dplyr::filter(relationship_to_ho_h == "Head of household's child") |>
        dplyr::distinct(household_id) |>
        dplyr::pull()

      parenting <- enrollment_data_filtered() |>
        dplyr::filter(
          household_id %in% hhs_with_children,
          relationship_to_ho_h %in% c(
            "Self (head of household)",
            "Head of household's spouse or partner"
          )
        ) |>
        dplyr::count(relationship_to_ho_h, name = "Count", sort = TRUE) |>
        dplyr::mutate(Percent = Count / n_youth_with_parenting_data())

    })

    # Create the {reactable} table to hold the parenting data
    output$parenting_tbl <- reactable::renderReactable(

      parenting_data() |>
        reactable::reactable(
          columns = list(
            relationship_to_ho_h = reactable::colDef(
              name = "Relationship to Head of Household",
              minWidth = 200,
              footer = "Total"
            ),
            Count = reactable::colDef(
              minWidth = 100,
              footer = function(values) sum(values)
            ),
            Percent = reactable::colDef(
              format = reactable::colFormat(
                percent = TRUE,
                digits = 2L
              ),
              minWidth = 100,
              footer = function(values) scales::percent(sum(values), accuracy = 0.01)
            )
          ),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold")
          )
        )

    )

  })
}

## To be copied in the UI
# mod_parenting_ui("parenting_1")

## To be copied in the server
# mod_parenting_server("parenting_1")
