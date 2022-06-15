#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    bslib::page_navbar(
      id = "nav_bar",
      title = span(
        img(src = "www/favicon.ico", height = 30),
        span(strong("RYHA"), style = "color: #ffffff")   # Predictive Ecology Yellow
      ),
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "zephyr",
        bg = "#ffffff",
        fg = "#000000",
        primary = ,
        base_font = bslib::font_google("Inter")
      ),

      bslib::nav(
        id = "overview_page",
        title = "Overview",

        shiny::p("Placeholder")

      ),

      bslib::nav(
        id = "gender_page",
        title = "Gender",

        shiny::p("Placeholder")

      ),

      bslib::nav(
        id = "ethnicity_page",
        title = "Ethnicity",

        shiny::p("Placeholder")

      ),

      bslib::nav(
        id = "veteran_page",
        title = "Veteran",

        shiny::p("Placeholder")

      ),

      bslib::nav(
        id = "disabilities_page",
        title = "Disabilities",

        shiny::p("Placeholder")

      ),

      bslib::nav(
        id = "employment_page",
        title = "Employment",

        shiny::p("Placeholder")

      ),

      bslib::nav(
        id = "education_page",
        title = "Education",

        shiny::p("Placeholder")

      ),

      bslib::nav(
        id = "exits_page",
        title = "Exits",

        shiny::p("Placeholder")

      ),

      bslib::nav(
        id = "health_page",
        title = "Disabilities",

        shiny::p("Placeholder")

      ),

      bslib::nav(
        id = "domestic_violence_page",
        title = "Domestic Violence",

        shiny::p("Placeholder")

      ),

      bslib::nav(
        id = "upload_page",
        title = "Upload Data",

        mod_upload_ui("upload_1")

      )

    )

  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ryha"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
