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

#' Create hyperlink to specific section in the HMIS Data Standards Manual
#'
#' This function generates a hyperlink to a specific section within the HMIS Data
#' Standards Manual. The link opens the manual in a new browser tab.
#'
#' @param section The name or number of the section to link to.
#' @param label The text to display as the link. Defaults to "HMIS Data Standards Manual".
#'
#' @return HTML hyperlink to the specified section in the HMIS Data Standards Manual.
#'
#' @examples
#' \dontrun{
#' link_section("R4 Last Grade Completed")
#' }
link_section <- function(section, label = "HMIS Data Standards Manual") {
  URL <- "https://files.hudexchange.info/resources/documents/HMIS-Data-Standards-Manual-2024.pdf"

  parsed_section <- section |>
    # %20 is how spaces are placed in URLs
    stringr::str_replace_all(" ", "%20")

  shiny::tags$a(
    href = glue::glue("{ URL }#{ parsed_section }"),
    target = "_blank",
    label
  )
}

filter_most_recent_data_collection_stage_per_enrollment <- function(data) {
  data |>
    dplyr::mutate(
      # Convert data_collection_stage to ordered factor
      data_collection_stage = factor(
        x = data_collection_stage,
        levels = c("Project start", "Project update", "Project annual assessment", "Project exit"),
        labels = c("Project start", "Project not start nor exit", "Project not start nor exit", "Project exit"),
        ordered = TRUE
      )
    ) |> 
    # Group by enrollment
    dplyr::group_by(enrollment_id, personal_id, organization_id) |>
    # Keep rows that correspond to the most recent data_collection_stage
    dplyr::filter(data_collection_stage == max(data_collection_stage)) |> 
    # Keep rows that have the most recent date_updated
    # (To handle multiple "Project not start nor exit" with different date_updated as most recent data_collection_stage)
    dplyr::filter(date_updated == max(date_updated)) |>
    # Handle multiple "Project not start nor exit" with the same date_updated as most recent data_collection_stage
    dplyr::slice(1) |>
    # Ungroup data
    dplyr::ungroup() 
}
