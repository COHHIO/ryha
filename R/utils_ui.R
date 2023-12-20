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
