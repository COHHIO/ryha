#' Create message with spinner
#'
#' @param message String. The message that should be placed below spinner
#'
#' @return HTML string that contains a spinner and text below it
spinner_message <- function(message) {
  shiny::tagList(
    waiter::spin_fading_circles(),
    message
  )
}

#' Add custom Bootstrap popover
#'
#' This function creates a Bootstrap popover containing additional information
#' to display alongside a given text.
#'
#' @param text The text to display.
#' @param title The title of the popover. Defaults to "More Info".
#' @param content The content to display inside the popover.
#' @param placement The placement of the popover relative to the text. Defaults to "right".
#'
#' @return HTML Bootstrap popover containing the specified content.
#'
#' @examples
#' \dontrun{
#' with_popover(
#'   text = "Some Example Text",
#'   content = shiny::tagList(
#'     shiny::span("First description line."),
#'     shiny::br(),
#'     shiny::span("Second description line.")
#'   )
#' )
#' }
with_popover <- function(text, title = "More Info", content, placement = "right") {

  bs4Dash::popover(
    tag = shiny::span(text, shiny::icon("question-circle")),
    content = content,
    title = title,
    placement = placement
  )

}

#' Create span with hyperlink to the HMIS Data Standards Manual
#'
#' `link_section()` generates a span that references a section and has a hyperlink to 
#' the HMIS Data Standards Manual. The link opens the manual in a new browser tab.
#'
#' @param section The name or number of the section to refer to.
#'
#' @return HTML span with a referenced section and a hyperlink to the HMIS Data Standards Manual.
#'
#' @examples
#' \dontrun{
#' link_section("R4 Last Grade Completed")
#' }
link_section <- function(section) {
  shiny::span("For more information, refer to section ", shiny::tags$b(section), " in the ", link_data_standards_manual())
}

#' Create a Link to the HMIS Data Standards Manual
#'
#' `link_data_standards_manual()` generates an HTML anchor (`<a>`) tag that links to the 
#' HMIS Data Standards Manual PDF.
#'
#' @return An HTML anchor (`<a>`) tag as a `shiny.tag` object.
#' @export
link_data_standards_manual <- function() {
  URL <- "https://cohhio.org/wp-content/uploads/2025/03/HMIS-Data-Standards-Manual-2024.pdf"
  shiny::tags$a(href = URL, target = "_blank", "HMIS Data Standards Manual")
}

#' Filter most recent data per enrollment
#'
#' `filter_most_recent_data_per_enrollment()` filters the most recent data
#' for enrollments with data collected in multiple data collection stages
#' following a set of rules.
#' 
#' @param data A data frame with multiple rows per enrollment
#' 
#' @details
#' An enrollment's most recent data corresponds to data collected at "Project exit".
#' If the enrollment has no "Project exit", the most recent data corresponds to data
#' collected at the "Project update" with the most recent date_updated ("Project update"
#' includes both "Project update" and "Project annual assessment" data_collection_stages). 
#' If an enrollment has multiple "Project update" with the same most recent date_updated, 
#' the tie is broken by selecting the first appearance. Finally, if an enrollment has no 
#' "Project update", the most recent data corresponds to data collected at "Project start".
#'
#' The general rules described above wouldn't be applicable in certain scenarios (e.g.
#' Employment data is collected at "Project start" and "Project exit" by definition,
#' showing missing data for entries that correspond to a "Project update"). To address
#' this issue, function's users are expected to preprocess the data accordingly before
#' calling `filter_most_recent_data_per_enrollment()` (e.g. by filtering out data that
#' correspond to "Project update" data collection stage).
#' 
#' @return A data frame with one row per enrollment
#' 
#' @examples 
#' \dontrun{
#' mock_data <- tibble::tribble(
#'   ~test_row, ~organization_id, ~personal_id, ~enrollment_id,  ~data_collection_stage,  ~date_updated, ~status,
#'   1, 1L, 1L, 1000L, "Project start", "2024-12-31", "A",
#'   2, 1L, 1L, 1000L, "Project update", "2023-01-01", "B"
#' ) |>
#'   dplyr::mutate(dplyr::across(date_updated, as.Date))
#' 
#' filter_most_recent_data_per_enrollment(mock_data)
#' }
filter_most_recent_data_per_enrollment <- function(data) {
  data |>
    dplyr::mutate(
      # Convert data_collection_stage to ordered factor
      data_collection_stage = factor(
        x = data_collection_stage,
        levels = c("Project start", "Project update", "Project annual assessment", "Project exit"),
        # Treat Project annual assessment as an update
        labels = c("Project start", "Project update", "Project update", "Project exit"),
        ordered = TRUE
      )
    ) |> 
    # Group by enrollment
    dplyr::group_by(enrollment_id, personal_id, organization_id) |>
    # Keep rows that correspond to the most recent data_collection_stage
    dplyr::filter(data_collection_stage == max(data_collection_stage)) |> 
    # Keep rows that have the most recent date_updated
    # (To handle multiple "Project update" with different date_updated as most recent data_collection_stage)
    dplyr::filter(date_updated == max(date_updated)) |>
    # Handle multiple "Project update" with the same date_updated as most recent data_collection_stage
    dplyr::slice(1) |>
    # Ungroup data
    dplyr::ungroup() 
}

#' Filter data based on clients and validate
#'
#' `filter_data()` filters the provided dataset to include only records that match 
#' the given `clients_filtered` dataset at the specified level (enrollment or youth). 
#' It then validates the filtered dataset to ensure it is not empty.
#'
#' @param data A data frame to be filtered
#' @param clients_filtered A data frame containing the client records 
#' to filter `data` by. Must contain columns that match the selected `at` level.
#' @param at A character string specifying the filtering level.
#' Either `"enrollment"` (default) or `"youth"`. 
#' - `"enrollment"`: Filters data based on `personal_id`, `organization_id` and `enrollment_id`.
#' - `"youth"`: Filters data based on `personal_id` and `organization_id`.
#'
#' @return A filtered data frame containing only the records matching the specified 
#' clients. If no matching records are found, a validation error is triggered.
filter_data <- function(data, clients_filtered, at = "enrollment") {
  by_cols <- switch(
    at,
    enrollment = c("personal_id", "organization_id", "enrollment_id"),
    youth = c("personal_id", "organization_id")
  )

  # Filter data
  filtered_data <- data |> 
    dplyr::semi_join(clients_filtered, by_cols)
  
  # Validate data
  validate_data(filtered_data)

  # Return data
  filtered_data
}

#' Validate data
#'
#' `validate_data()` checks whether the provided dataset has at least one row.
#' If the data is empty, it triggers a validation error with a specified message.
#' 
#' @param data A data frame to validate
#' @param message A character string specifying the message to display when 
#' the data is empty. Defaults to "No data to display".
#' 
#' @return This function does not return a value. It triggers a validation 
#' error in a Shiny app if the dataset is empty, preventing further execution.
validate_data <- function(data, message = "No data to display") {
  shiny::validate(
    shiny::need(
      expr = nrow(data) >= 1L,
      message = message
    )
  )
}
