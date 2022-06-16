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
  purrr::map2(
    .x = menuItems$text,
    .y = menuItems$tabName,
    .f = function(x, y){
      bs4Dash::menuItem(
        text    = x,
        tabName = y
      )
    }
  )
}
