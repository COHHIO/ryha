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

with_popover <- function(text, title = "More Info", content, placement = "right") {

  bs4Dash::popover(
    tag = shiny::span(text, shiny::icon("question-circle")),
    content = content,
    title = title,
    placement = placement)

}

link_section <- function(section, label = "HMIS Data Standars Manual") {
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
