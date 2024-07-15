#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Print environment (for debugging purposes)
  cli::cli_alert_info("Running in '{Sys.getenv('APP_BACKEND')}' mode")

  # Create dm object. This is run once per session
  dm <- create_dm(env = Sys.getenv("APP_BACKEND"))

  # Run data filtering and server modules only when data is available
  if (nrow(dm$client) > 0) {

    # Create a reactiveValues list to hold some global variables
    rctv <- shiny::reactiveValues()

    # Get filtered dm
    clients_filtered <- mod_filters_server(
      id = "filters_1",
      dm = dm,
      w = w,
      rctv = rctv
    )

    mod_overview_server(
      id = "overview_1",
      client_data = dm$client,
      gender_data = dm$gender,
      enrollment_data = dm$enrollment,
      ethnicity_data = dm$ethnicity,
      clients_filtered = clients_filtered
    )

    mod_disabilities_server(
      id = "disabilities_1",
      disabilities_data = dm$disabilities,
      clients_filtered = clients_filtered
    )

    mod_employment_server(
      id = "employment_1",
      employment_data = dm$employment,
      clients_filtered = clients_filtered
    )

    mod_education_server(
      id = "education_1",
      education_data = dm$education,
      clients_filtered = clients_filtered
    )

    mod_health_server(
      id = "health_1",
      health_data = dm$health,
      counseling_data = dm$exit,
      clients_filtered = clients_filtered
    )

    mod_domestic_violence_server(
      id = "domestic_violence_1",
      domestic_violence_data = dm$domestic_violence,
      clients_filtered = clients_filtered
    )

    mod_income_benefits_server(
      id = "income_benefits_1",
      income_data = dm$income,
      benefits_data = dm$benefits,
      clients_filtered = clients_filtered
    )

    mod_services_server(
      id = "services_1",
      services_data = dm$services,
      referral_data = dm$enrollment,
      clients_filtered = clients_filtered
    )

    mod_trafficking_server(
      id = "trafficking_1",
      trafficking_data = dm$exit,
      clients_filtered = clients_filtered
    )

    mod_living_situation_server(
      id = "living_situation_1",
      project_data = dm$project,
      enrollment_data = dm$enrollment,
      exit_data = dm$exit,
      clients_filtered = clients_filtered,
      rctv = rctv
    )

    mod_parenting_server(
      id = "parenting_1",
      health_data = dm$health,
      enrollment_data = dm$enrollment,
      clients_filtered = clients_filtered
    )

    mod_exit_server(
      id = "exit_1",
      exit_data = dm$exit,
      clients_filtered = clients_filtered
    )

  } else {

    # Hide filtering and navigation elements when there is no data available

    # List ids of elements to hide
    c(
      "tab-overview_page",
      "tab-disabilities_page",
      "tab-employment_page",
      "tab-education_page",
      "tab-services_page",
      "tab-health_page",
      "tab-domestic_violence_page",
      "tab-income_benefits_page",
      "tab-trafficking_page",
      "tab-living_situation_page",
      "tab-parenting_page",
      "tab-exit_page",
      "tab-help_page",
      "controlbar-toggle"
    ) |>
      # Hide elements
      purrr::map(function(id) shinyjs::hide(id))
  }

  # Upload module should always be present, regardless of data availability
  mod_upload_server(
    id = "upload_1"
  )

  # Define logic to navigate to "Help Page" when the link is clicked
  # input$to_help is defined using JavaScript
  shiny::observeEvent(input$to_help, {
    bs4Dash::updateTabItems(
      session = session,
      inputId = "left_sidebar_menu",
      selected = "help_page"
    )
  })
}
