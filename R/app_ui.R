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
        bootswatch = "simplex",
        bg = "#ffffff",
        fg = "#000000",
        primary = ,
        base_font = bslib::font_google("Inter")
      ),

      bslib::nav(
        id = "overview_page",
        title = "Overview",

        mod_overview_ui("overview_1")

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
