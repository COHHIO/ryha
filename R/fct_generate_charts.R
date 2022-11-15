


#' generate_charts
#'
#' @param data
#' @param category
#' @param count
#'
#' @description A fct function
#'
#' @return An {echarts4r} interactive pie chart
#'
#' @noRd
#'
#' @examples
#' pie_chart(
#'   data = dplyr::count(iris, Species),
#'   category = "Species",
#'   count = "n"
#' )
pie_chart <- function(data, category, count) {

  data |>
    echarts4r::e_charts_(x = category) |>
    echarts4r::e_pie_(
      serie = count,
      name = category |> janitor::make_clean_names(case = "title"),
      legend = TRUE,
      label = list(
        show = TRUE,
        position = "inside",
        formatter = "{c}"   # show the numeric value as the label
      ),
      radius = c("50%", "70%"),
      # emphasize the label when hovered over
      emphasis = list(
        label = list(
          show = TRUE,
          fontSize = "15",
          fontWeight = "bold"
        )
      )
    ) |>
    echarts4r::e_legend(bottom = 0) |>   # place legend below chart
    echarts4r::e_title(
      subtext = "Chart represents most recent data\nfor each individual"
    ) |>
    echarts4r::e_tooltip(trigger = "item") |>
    echarts4r::e_grid(containLabel = TRUE) |>
    echarts4r::e_show_loading()

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


sankey_chart <- function() {

  # data.frame(
  #   source = c("a_start", "b_start", "c_start", "b_start", "c_start"),
  #   target = c("b_end", "b_end", "a_end", "c_end", "c_end"),
  #   value = ceiling(rnorm(5, 10, 1)),
  #   stringsAsFactors = FALSE
  # ) |>
  #   echarts4r::e_charts() |>
  #   echarts4r::e_sankey(source, target, value) |>
  #   echarts4r::e_tooltip(trigger = "item") |>
  #   echarts4r::e_title("Sankey chart")

  # read_health() |>
  #   # do data prep to get just one observation at entry and one observation at
  #   # exit, by individual (keeping only individuals who exited)
  # migrate::migrate(
  #   data = for_migrate,
  #   id = personal_id,
  #   time = information_date,
  #   state = general_health_status,
  #   percent = FALSE
  # ) |>
  #   dplyr::mutate(
  #     general_health_status_start = paste0(general_health_status_start, " (Entry)"),
  #     general_health_status_end = paste0(general_health_status_end, " (Exit)")
  #   ) |>
  #   dplyr::filter(count > 0L) |>
  #   echarts4r::e_charts() |>
  #   echarts4r::e_sankey(source = general_health_status_start, target = general_health_status_end, value = count) |>
  #   echarts4r::e_tooltip()

}

