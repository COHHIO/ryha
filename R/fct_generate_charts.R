


#' generate_charts
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd



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



generate_bar_chart <- function(data, group) {

  data |>
    dplyr::count(
      .data[[group]],
      name = "Count"
    ) |>
    dplyr::arrange(Count) |>
    echarts4r::e_charts_(x = group) |>
    echarts4r::e_bar(
      serie = Count,
      name = "# of Youth",
      legend = FALSE,
      label = list(
        formatter = '{@[0]}',
        show = TRUE,
        position = "right"
      )
    ) |>
    echarts4r::e_flip_coords()

}
