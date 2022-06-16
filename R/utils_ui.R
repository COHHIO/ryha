#' Create menuItems based on a dataset
#'
#' This function uses Functional Programming Tools to generate multiple elements
#'  with the same structure. The dataset contains one column per parameter used.
#'
#' @param menuItems A tibble with columns `text` and `tabName`
#'
#' @return
#' @export
create_menuItems <- function(menuItems){
  purrr::pmap(
    .l = list(menuItems$text, menuItems$tabName, menuItems$icon),
    .f = function(text, tabName, icon) {
      bs4Dash::menuItem(
        text = text,
        tabName = tabName,
        icon = shiny::icon(icon)
      )
    }
  )
}
