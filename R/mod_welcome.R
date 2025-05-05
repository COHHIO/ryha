#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import markdown
#' @importFrom shiny NS tagList
mod_welcome_ui <- function(id) {
    ns <- NS(id)
    tagList(
        bslib::layout_columns(
            col_widths = c(-2, 8, -2),
            bslib::card(
                bslib::card(
                    bslib::card_header(shiny::HTML("<h1>Welcome to the <em>Youth Homelessness Data Dashboard</em></h1>")),
                    bslib::layout_columns(
                        col_widths = c(3, 6, 3),
                        class = "vertical-center",
                        shiny::img(src = "www/odh_logo.png", width = "100%"),
                        shiny::img(src = "www/cohhio_logo.png", width = "100%"),
                        shiny::img(src = "www/ketchbrook_logo.png", width = "100%")
                    )
                ),
                bslib::layout_columns(
                    bslib::card(
                        bslib::card(
                            bslib::card_header(shiny::h2("Partnership")),
                            shiny::markdown("This app is the result of a partnership between:
                - [Ohio Department of Health (ODH)](https://odh.ohio.gov/)
                - [Coalition on Homelessness and Housing in Ohio (COHHIO)](https://cohhio.org/)
                - [Ketchbrook Analytics](https://www.ketchbrookanalytics.com)")
                        ) |>
                            shiny::tagAppendAttributes(class = "custom-box"),
                        bslib::card(
                            bslib::card_header(shiny::h2("Contact & Help")),
                            shiny::markdown("This app utilizes the HMIS CSV to provide information on the ODH program. If you have any questions about the app, please email Amanda Wilson, Youth Housing Initiative Director at COHHIO, at [amandawilson@cohhio.org](mailto:amandawilson@cohhio.org)"),
                            shiny::div("Please refer to ", shiny::actionLink(inputId = "to_help", label = "Help"), " page for more information on how to navigate the app.")
                        ) |>
                            shiny::tagAppendAttributes(class = "custom-box")
                    ),
                    bslib::card(
                        bslib::card_header(shiny::h2("Program Development")),
                        shiny::markdown("As part of ODH's youth homelessness grant program, **COHHIO** was contracted to assist the department in program development using data-driven approaches."),
                        shiny::markdown("**COHHIO** supported the department in establishing data collection requirements and providing technical assistance to sub-recipients to standardize data collection across the program."),
                        shiny::markdown("This was best accomplished using the **Housing and Urban Development** (HUD)-required **Homeless Management Information System** (HMIS)."),
                        shiny::markdown("In 2022, **COHHIO** entered into a contract with [Ketchbrook Analytics](https://www.ketchbrookanalytics.com) to assist by analyzing large volumes of data from sub-grantees and quantifying trends across data sets.")
                    ) |>
                        shiny::tagAppendAttributes(class = "custom-box")
                )
            )
        )
    )
}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
    })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
