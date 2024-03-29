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

      help = NULL,

      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "Youth Data Dashboard",
          color = "secondary",
          href = "https://cohhio.org/youthhealth/",
          image = "www/favicon.ico"
        ),
        controlbarIcon = shiny::icon("filter")
      ),

      sidebar = bs4Dash::dashboardSidebar(

        bs4Dash::sidebarMenu(
          id = "left_sidebar_menu",
          # Sidebar menu items are created in R/fct_menuItems.R
          create_menuItems(define_menu())
        )

      ),

      controlbar = bs4Dash::bs4DashControlbar(
        mod_filters_ui("filters_1"),
        id = "control_bar",
        width = 500,
        collapsed = FALSE
      ),

      body = bs4Dash::dashboardBody(

        bs4Dash::tabItems(

          bs4Dash::tabItem(
            tabName = "welcome_page",
            mod_welcome_ui("welcome_1")
          ),

          bs4Dash::tabItem(
            tabName = "overview_page",
            mod_overview_ui("overview_1")
          ),

          bs4Dash::tabItem(
            tabName = "disabilities_page",
            mod_disabilities_ui("disabilities_1")
          ),

          bs4Dash::tabItem(
            tabName = "employment_page",
            mod_employment_ui("employment_1")
          ),

          bs4Dash::tabItem(
            tabName = "education_page",
            mod_education_ui("education_1")
          ),

          bs4Dash::tabItem(
            tabName = "services_page",
            mod_services_ui("services_1")
          ),

          bs4Dash::tabItem(
            tabName = "health_page",
            mod_health_ui("health_1")
          ),

          bs4Dash::tabItem(
            tabName = "domestic_violence_page",
            mod_domestic_violence_ui("domestic_violence_1")
          ),

          bs4Dash::tabItem(
            tabName = "income_benefits_page",
            mod_income_benefits_ui("income_benefits_1")
          ),

          bs4Dash::tabItem(
            tabName = "trafficking_page",
            mod_trafficking_ui("trafficking_1")
          ),

          bs4Dash::tabItem(
            tabName = "living_situation_page",
            mod_living_situation_ui("living_situation_1")
          ),

          bs4Dash::tabItem(
            tabName = "parenting_page",
            mod_parenting_ui("parenting_1")
          ),

          bs4Dash::tabItem(
            tabName = "exit_page",
            mod_exit_ui("exit_1")
          ),

          bs4Dash::tabItem(
            tabName = "upload_page",
            mod_upload_ui("upload_1")
          ),

          bs4Dash::tabItem(
            tabName = "help_page",

            shiny::fluidRow(
              shiny::column(width = 2),

              shiny::column(
                width = 8,
                shiny::includeMarkdown( app_sys("app/www/help_text.md") )
              ) |>
                shiny::tagAppendAttributes(class = "help-page-content"),

              shiny::column(width = 2)

            )

          )

        )

      ),

      # remove ability to toggle between "light" and "dark" mode
      dark = NULL,

      # Show {waiter} on load
      preloader = list(
        html = shiny::tagList(
          waiter::spin_fading_circles(),
          "Welcome!",
          shiny::br(),
          "Data is loading, please wait..."
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
      app_title = "COHHIO Youth Data Dashboard"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    waiter::useWaiter()
  )
}
