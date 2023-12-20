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

      shiny::column(
        width = 4,
        # Number of clients (post filters)
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_box"),
          width = "100%"
        )
      ),

      shiny::column(
        width = 4,
        # Number of youth with pregnancy data
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_pregnancy_data_box"),
          width = "100%"
        )
      ),

      shiny::column(
        width = 4,
        # Number of youth with parenting data
        bs4Dash::bs4ValueBoxOutput(
          outputId = ns("n_youth_with_parenting_data_box"),
          width = "100%"
        )
      )

    ),

    shiny::hr(),

    shiny::fluidRow(

      # Pregnancy Pie Chart ----
      shiny::column(
        width = 6,

        bs4Dash::box(
          title = "# of Youth by Pregnancy Status",
          width = NULL,
          maximizable = TRUE,
          echarts4r::echarts4rOutput(
            outputId = ns("pregnancy_pie_chart")
          )
        )

      ),

      shiny::column(
        width = 6,

        # Parenting Table ----
        bs4Dash::box(
          title = "# of Youth Parenting",
          width = NULL,
          maximizable = TRUE,
          reactable::reactableOutput(
            outputId = ns("parenting_tbl")
          )
        ),

        # Data Quality Statistics ----
        bs4Dash::tabsetPanel(
          type = "pills",

          ## Pregnancy ----
          shiny::tabPanel(
            title = "Pregnancy",

            bs4Dash::box(
              title = "Pregnancy Status Data Quality Statistics",
              width = NULL,
              maximizable = TRUE,
              reactable::reactableOutput(
                outputId = ns("pregnancy_missingness_stats_tbl")
              )
            )

          ),

          ## Parenting ----
          shiny::tabPanel(
            title = "Parenting",

            bs4Dash::box(
              title = "Parenting Status Data Quality Statistics",
              width = NULL,
              maximizable = TRUE,
              reactable::reactableOutput(
                outputId = ns("parenting_missingness_stats_tbl")
              )
            )

          )

        )

      )

    )

  )
}

#' parenting Server Functions
#'
#' @noRd
mod_parenting_server <- function(id, health_data, enrollment_data, clients_filtered){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Info Boxes ----

    # Total number of Youth in program(s), based on `client.csv` file
    n_youth <- shiny::reactive(

      clients_filtered() |>
        nrow()

    )

    # Apply the filters to the trafficking data
    health_data_filtered <- shiny::reactive(

      health_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

    )

    # Apply the filters to the trafficking data
    enrollment_data_filtered <- shiny::reactive(

      enrollment_data |>
        dplyr::inner_join(
          clients_filtered(),
          by = c("personal_id", "organization_id")
        )

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

    # Render number of clients box
    output$n_youth_box <- bs4Dash::renderbs4ValueBox(

      bs4Dash::bs4ValueBox(
        value = n_youth(),
        subtitle = "Total # of Youth in Program(s)",
        icon = shiny::icon("user", class = "fa-solid")
      )

    )

    # Render "number youth with pregnancy data available" box
    output$n_youth_with_pregnancy_data_box <- bs4Dash::renderbs4ValueBox(

      bs4Dash::bs4ValueBox(
        value = n_youth_with_pregnancy_data(),
        subtitle = "Total # of Youth with Pregnancy Data Available",
        icon = shiny::icon("baby-carriage")
      )

    )

    # Render "number youth with parenting data available" box
    output$n_youth_with_parenting_data_box <- bs4Dash::renderbs4ValueBox(

      bs4Dash::bs4ValueBox(
        value = n_youth_with_parenting_data(),
        subtitle = "Total # of Youth with Parenting Data Available",
        icon = shiny::icon("baby-carriage")
      )

    )

    # Pregnancy Status ----

    ## Pie Chart ----

    # Create reactive data frame to data to be displayed in pie chart
    pregnancy_pie_chart_data <- shiny::reactive({

      shiny::validate(
        shiny::need(
          expr = nrow(health_data_filtered()) >= 1L,
          message = "No data to display"
        )
      )

      out <- health_data_filtered() |>
        dplyr::filter(
          pregnancy_status %in% c("Yes", "No")
        ) |>
        dplyr::arrange(
          organization_id,
          personal_id,
          pregnancy_status,
          dplyr::desc(date_updated)
        ) |>
        dplyr::select(
          organization_id,
          personal_id,
          pregnancy_status
        ) |>
        dplyr::distinct(
          organization_id,
          personal_id,
          pregnancy_status,
          .keep_all = TRUE
        )

      shiny::validate(
        shiny::need(
          expr = nrow(out) >= 1L,
          message = "No data to display"
        )
      )

      out |>
        dplyr::count(pregnancy_status) |>
        dplyr::arrange(pregnancy_status)

    })

    # Create pregnancy status pie chart
    output$pregnancy_pie_chart <- echarts4r::renderEcharts4r({

      pregnancy_pie_chart_data() |>
        pie_chart(
          category = "pregnancy_status",
          count = "n"
        )

    })

    ## Data Quality Statistics ----

    # Capture the data quality statistics for "mental_health_status" field
    pregnancy_missingness_stats <- shiny::reactive(

      health_data_filtered() |>
        dplyr::mutate(pregnancy_status = ifelse(
          is.na(pregnancy_status),
          "(Blank)",
          pregnancy_status
        )) |>
        dplyr::filter(
          pregnancy_status %in% c(
            "Client doesn't know",
            "Client prefers not to answer",
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(pregnancy_status, name = "Count") |>
        dplyr::rename(Response = pregnancy_status)

    )

    # Create the {reactable} table to hold the missingness stats
    output$pregnancy_missingness_stats_tbl <- reactable::renderReactable(
      reactable::reactable(
        pregnancy_missingness_stats()
      )
    )

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

    ## Data Quality Statistics Table ----
    output$parenting_missingness_stats_tbl <- reactable::renderReactable(

      enrollment_data_filtered() |>
        dplyr::mutate(relationship_to_ho_h = ifelse(
          is.na(relationship_to_ho_h),
          "(Blank)",
          relationship_to_ho_h
        )) |>
        dplyr::filter(
          relationship_to_ho_h %in% c(
            "Data not collected",
            "(Blank)"
          )
        ) |>
        dplyr::count(relationship_to_ho_h, name = "Count") |>
        dplyr::rename(Response = relationship_to_ho_h) |>
        reactable::reactable()

    )

  })
}

## To be copied in the UI
# mod_parenting_ui("parenting_1")

## To be copied in the server
# mod_parenting_server("parenting_1")
