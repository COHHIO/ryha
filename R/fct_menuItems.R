


#' Define List of Menu Items
#'
#' @return A tibble, containing the menu title, id, and icon
#'
#' @details Note: Font-Awesome v5 is used for the icons
#'
#' @noRd
define_menu <- function() {

  tibble::tribble(
    ~text              , ~tabName                , ~icon,
    "Welcome"          , "welcome_page"          , "hand-paper",
    "Overview"         , "overview_page"         , "home",
    # "Client"           , "client_page"           , "user",
    # "Living Situation" , "living_situation_page" , "home",
    "Disabilities"     , "disabilities_page"     , "accessible-icon",
    "Employment"       , "employment_page"       , "briefcase",
    "Education"        , "education_page"        , "book-open",
    # "Exits"            , "exits_page"            , "door-open",
    "Services"         , "services_page"         , "hands-helping",
    "Health"           , "health_page"           , "stethoscope",
    "Domestic Violence", "domestic_violence_page", "user-shield",
    "Benefits"         , "benefits_page"         , "hand-holding-medical",
    "Upload Data"      , "upload_page"           , "upload"
  )

}



#' Create {bs4Dash} `menuItems` based on a dataset
#'
#' @details The input dataset to this function should be managed by the custom
#'   `define_menu()` function
#'
#' @param menuItems A tibble with columns `text`, `tabName`, and `icon`; this
#'   tibble should be the result of calling `define_menu()`
#'
#' @noRd
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
