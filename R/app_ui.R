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
    bs4Dash::dashboardPage(
      header = bs4Dash::dashboardHeader(
        title = span(
          img(src = "www/favicon.ico", height = 30),
          span(strong("RYHA"), style = "color: #ffffff")
        )
      ),
      sidebar = bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
          # menuItems is created in R/fct_menuItems.R
          create_menuItems(define_menu())
        )
      ),
      controlbar = bs4Dash::bs4DashControlbar(
        shiny::p("Place Filters Here!"),
        id = "control_bar",
        width = 350,
        collapsed = FALSE
      ),
      body = bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "overview_page",
            shiny::p("Placeholder Overview")
          ),
          bs4Dash::tabItem(
            tabName = "client_page",
            shiny::p("Placeholder")
          ),
          bs4Dash::tabItem(
            tabName = "disabilities_page",
            shiny::p("Placeholder Disabilities")
          ),
          bs4Dash::tabItem(
            tabName = "employment_page",
            shiny::p("Placeholder Employment")
          ),
          bs4Dash::tabItem(
            tabName = "education_page",
            shiny::p("Placeholder Education")
          ),
          bs4Dash::tabItem(
            tabName = "exits_page",
            shiny::p("Placeholder Exits")
          ),
          bs4Dash::tabItem(
            tabName = "health_page",
            shiny::p("Placeholder Disabilities")
          ),
          bs4Dash::tabItem(
            tabName = "domestic_violence_page",
            shiny::p("Placeholder Domestic Violence")
          ),
          bs4Dash::tabItem(
            tabName = "upload_page",
            mod_upload_ui("upload_1")
          )
        )
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
      app_title = "COHHIO Youth Homelessness Analyzer"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
