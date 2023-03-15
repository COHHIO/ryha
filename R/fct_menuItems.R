


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
    "Overview"         , "overview_page"         , "eye",
    "Disabilities"     , "disabilities_page"     , "accessible-icon",
    "Employment"       , "employment_page"       , "briefcase",
    "Education"        , "education_page"        , "book-open",
    "Services"         , "services_page"         , "hands-helping",
    "Health"           , "health_page"           , "stethoscope",
    "Domestic Violence", "domestic_violence_page", "user-shield",
    "Income & Benefits", "income_benefits_page"  , "dollar-sign",
    "Trafficking"      , "trafficking_page"      , "exclamation-circle",
    "Living Situation" , "living_situation_page" , "bed",
    "Parenting"        , "parenting_page"        , "baby-carriage",
    "Exit"             , "exit_page"             , "door-open",
    "Upload Data"      , "upload_page"           , "upload",
    "Help"             , "help_page"             , "question"
  )

}



#' Create {bs4Dash} `menuItems` based on a data frame
#'
#' @details The input data frame to this function should be managed by the custom
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
