#' value_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_value_box_ui <- function(id, title, tooltip = NULL) {
    ns <- NS(id)

    title_ui <- if (!is.null(tooltip)) {
        bslib::tooltip(
            shiny::span(
                title,
                bsicons::bs_icon("info-circle")
            ),
            tooltip,
            placement = "right"
        )
    } else {
        title
    }

    tagList(
        bslib::value_box(
            title = title_ui,
            value = shiny::textOutput(ns("value"))
        )
    )
}

#' value_box Server Functions
#'
#' @noRd
mod_value_box_server <- function(id, rctv_data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$value <- shiny::renderText({
            scales::label_comma()(nrow(rctv_data()))
        })
    })
}

## To be copied in the UI
# mod_value_box_ui("value_box_1")

## To be copied in the server
# mod_value_box_server("value_box_1")
