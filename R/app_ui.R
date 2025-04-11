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

    waiter::waiter_show_on_load(
      html = shiny::tagList(
        waiter::spin_fading_circles(),
        "Welcome!",
        shiny::br(),
        "Data is loading, please wait..."
      )
    ),

    # Your application UI logic
    bslib::page_navbar(
      id = "navbar",
      fillable = FALSE,

      title = shiny::span(
        shiny::tags$a(
          href = "https://cohhio.org/youthhealth/",
          target = "_blank",
          style = "text-decoration: none;",
          shiny::tags$img(
            src = "www/favicon.ico",
            height = "40px",
            style = "margin-right: 8px;"
          )
        ),
        "Youth Data Dashboard"
      ),
      bslib::nav_panel(
        title = shiny::span(shiny::icon("hand-paper"), "Welcome"),
        mod_welcome_ui("welcome_1")
      ),

      bslib::nav_panel(
        title = shiny::span(shiny::icon("magnifying-glass-chart"), "Explore Data"),

        bslib::navset_card_underline(
          sidebar = bslib::sidebar(
            width = 450,
            position = "right",
            mod_filters_ui("filters_1")
          ),
          bslib::nav_panel(
            title = get_nav_panel_title("Overview", "eye"),
            mod_overview_ui("overview_1")
          ),
          bslib::nav_panel(
            title = get_nav_panel_title("Disabilities", "accessible-icon"),
            mod_disabilities_ui("disabilities_1")
          ),
          bslib::nav_panel(
            title = get_nav_panel_title("Employment", "briefcase"),
            mod_employment_ui("employment_1")
          ),
          bslib::nav_panel(
            title = get_nav_panel_title("Education", "book-open"),
            mod_education_ui("education_1")
          ),
          bslib::nav_panel(
            title = get_nav_panel_title("Services", "hands-helping"),
            mod_services_ui("services_1")
          ),
          bslib::nav_panel(
            title = get_nav_panel_title("Health", "stethoscope"),
            mod_health_ui("health_1")
          ),
          bslib::nav_panel(
            title = get_nav_panel_title("Domestic Violence", "user-shield"),
            mod_domestic_violence_ui("domestic_violence_1")
          ),
          bslib::nav_panel(
            title = get_nav_panel_title("Income & Benefits", "dollar-sign"),
            mod_income_benefits_ui("income_benefits_1")
          ),
          bslib::nav_panel(
            title = get_nav_panel_title("Trafficking", "exclamation-circle"),
            mod_trafficking_ui("trafficking_1")
          ),
          bslib::nav_panel(
            title = get_nav_panel_title("Living Situation", "bed"),
            mod_living_situation_ui("living_situation_1")
          ),
          bslib::nav_panel(
            title = get_nav_panel_title("Parenting", "baby-carriage"),
            mod_parenting_ui("parenting_1")
          ),
          bslib::nav_panel(
            title = get_nav_panel_title("Exit", "door-open"),
            mod_exit_ui("exit_1")
          )
        ) |>
          shiny::tagAppendAttributes(class = "nav-justified")
      ),
      bslib::nav_panel(
        title = shiny::span(shiny::icon("upload"), "Upload Data"),
        mod_upload_ui("upload_1")
      ),

      bslib::nav_panel(
        title = shiny::span(shiny::icon("question"), "Help"),
        value = "Help",
        "Section under development"
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
    waiter::useWaiter(),
    shiny::useBusyIndicators()
  )
}
