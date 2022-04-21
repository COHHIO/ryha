#' read_data
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
read_client <- function() {

  readr::read_csv(
    file = here::here("data/Client.csv"),
    col_select = c(PersonalID, DOB, AmIndAKNative:GenderNone),
    col_types = readr::cols(
      .default = readr::col_integer(),
      PersonalID = readr::col_character(),
      DOB = readr::col_date()
    )
  ) |>
    dplyr::rename(HispanicLatino = Ethnicity)

}



prep_heatmap <- function(data) {

  data |>
    dplyr::select(-DOB) |>
    tidyr::pivot_longer(-PersonalID) |>
    dplyr::filter(value != 0) %>%
    dplyr::left_join(x = ., y = ., by = "PersonalID") |>
    dplyr::group_by(name.x, name.y) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(n = ifelse(name.x == name.y, NA, n))

}



prep_bar_chart <- function(data) {

  data |>
    dplyr::select(-DOB) |>
    dplyr::select(PersonalID, AmIndAKNative:HispanicLatino) |>
    tidyr::pivot_longer(
      cols = -PersonalID,
      names_to = "Ethnicity",
      values_to = "Status",
      values_transform = list(Status = as.integer)
    ) |>
    dplyr::filter(Status == 1L) |>
    dplyr::count(
      Ethnicity,
      name = "Count"
    ) |>
    dplyr::arrange(Count)

}
