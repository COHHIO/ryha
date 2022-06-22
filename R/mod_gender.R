#' gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidRow(

      shiny::column(
        width = 6,
        shiny::h4("Grantee: YWCA")
      ),

      shiny::column(
        width = 6,
        shiny::h4("Period: Jan 1, 2022 - March 31, 2022")
      )

    ),

    shiny::fluidRow(

      shiny::column(
        width = 4,

        bs4Dash::box(
          width = 12,
          title = "Male Participants",
          background = "olive",

          bs4Dash::descriptionBlock(
            number = shiny::textOutput(outputId = ns("male_pct_txt")),
            numberColor = "white",
            header = shiny::textOutput(outputId = ns("male_count_txt")),
            rightBorder = FALSE,
            marginBottom = FALSE
          )

        )

      ),

      shiny::column(
        width = 4,

        bs4Dash::box(
          width = 12,
          title = "Female Participants",
          background = "info",

          bs4Dash::descriptionBlock(
            number = shiny::textOutput(outputId = ns("female_pct_txt")),
            numberColor = "white",
            header = shiny::textOutput(outputId = ns("female_count_txt")),
            rightBorder = FALSE,
            marginBottom = FALSE
          )

        )

      ),

      shiny::column(
        width = 4,

        bs4Dash::box(
          width = 12,
          title = "Other Gender Participants",
          background = "indigo",

          bs4Dash::descriptionBlock(
            number = shiny::textOutput(outputId = ns("other_pct_txt")),
            numberColor = "white",
            header = shiny::textOutput(outputId = ns("other_count_txt")),
            rightBorder = FALSE,
            marginBottom = FALSE
          )

        )

      )

    ),

    shiny::fluidRow(

      shiny::column(
        width = 12,

        bs4Dash::box(
          width = 12,
          title = "# of Participants By Gender Identification",
          echarts4r::echarts4rOutput(outputId = ns("gender_chart"))
        )

      )

    )

  )
}

#' gender Server Functions
#'
#' @noRd
mod_gender_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$male_pct_txt <- shiny::renderText({

      male <- data$gender |>
        dplyr::filter(Gender == "Male") |>
        dplyr::collect() |>
        nrow()

      paste0(
        round(male / data$gender$num_rows * 100, 2),
        "%"
      )

    })

    output$male_count_txt <- shiny::renderText({

      data$gender |>
        dplyr::filter(Gender == "Male") |>
        dplyr::collect() |>
        nrow()

    })

    output$female_pct_txt <- shiny::renderText({

      female <- data$gender |>
        dplyr::filter(Gender == "Female") |>
        dplyr::collect() |>
        nrow()

      paste0(
        round(female / data$gender$num_rows * 100, 2),
        "%"
      )

    })

    output$female_count_txt <- shiny::renderText({

      data$gender |>
        dplyr::filter(Gender == "Female") |>
        dplyr::collect() |>
        nrow()

    })

    output$other_pct_txt <- shiny::renderText({

      other <- data$gender |>
        dplyr::filter(!Gender %in% c("Male", "Female")) |>
        dplyr::collect() |>
        nrow()

      paste0(
        round(other / data$gender$num_rows * 100, 2),
        "%"
      )

    })

    output$other_count_txt <- shiny::renderText({

      data$gender |>
        dplyr::filter(!Gender %in% c("Male", "Female")) |>
        dplyr::collect() |>
        nrow()

    })

    output$gender_chart <- echarts4r::renderEcharts4r({

      data$gender |>
        dplyr::collect() |>
        dplyr::group_by(Gender) |>
        dplyr::summarise(Count = dplyr::n(), .groups = "drop") |>
        echarts4r::e_charts(x = Gender) |>
        echarts4r::e_bar(serie = Count) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_tooltip(trigger = "item")

    })

  })
}

## To be copied in the UI
# mod_gender_ui("gender_1")

## To be copied in the server
# mod_gender_server("gender_1")
