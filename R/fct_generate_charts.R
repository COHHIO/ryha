#' Generate a pie chart using echarts4r
#'
#' This function generates a pie chart using the echarts4r package.
#'
#' @param data A data frame containing the data to be plotted.
#' @param category A character string specifying the column name in the data
#' frame representing the categories for the pie chart slices.
#' @param count A character string specifying the column name in the data frame
#' representing the counts or values associated with each category.
#'
#' @return A pie chart visualized using echarts4r.
#'
#' @examples
#' \dontrun{
#' mock_data <- data.frame(x = c("A", "B", "C"), y = c(10, 20, 30))
#' pie_chart(
#'   data = mock_data,
#'   category = "x",
#'   count = "y"
#' )
#' }
pie_chart <- function(data, category, count) {
  data |>
    # echarts4r::e_charts_() allows `x` to be a character string
    echarts4r::e_charts_(x = category) |>
    echarts4r::e_pie_(
      serie = count,
      name = category |> janitor::make_clean_names(case = "title"),
      legend = TRUE,
      label = list(
        show = TRUE,
        position = "outside",
        formatter = "{d}%"   # show the percentage as the label
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
    echarts4r::e_tooltip(trigger = "item") |>
    echarts4r::e_grid(
      top = "10",
      left = "60",
      right = "60",
      containLabel = TRUE
    ) |>
    echarts4r::e_show_loading()

}

#' Generate a bar chart using echarts4r
#'
#' This function generates a bar chart using the echarts4r package.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x A character string specifying the column name in the data frame
#' representing the x-axis values.
#' @param y A character string specifying the column name in the data frame
#' representing the y-axis values.
#' @param pct_denominator Optional numeric value specifying the denominator 
#' for percentage calculation.
#' @param axis_flip A logical value indicating whether to flip the x and y axes.
#' Default is TRUE.
#'
#' @return A bar chart visualized using echarts4r.
#'
#' @examples
#' \dontrun{
#' mock_data <- data.frame(x = c("A", "B", "C"), y = c(10, 20, 30))
#' bar_chart(
#'   data = mock_data,
#'   x = "x",
#'   y = "y"
#' )
#' }
bar_chart <- function(data, x, y, pct_denominator = NULL, axis_flip = TRUE) {

  # Calculate percentage column
  if (!is.null(pct_denominator)) {
    data <- data |> 
      dplyr::mutate(pct = n / pct_denominator)
  } else {
    data <- data |> 
      dplyr::mutate(pct = n / sum(n))
  }

  out <- data |>
    echarts4r::e_charts_(x = x) |>
    echarts4r::e_bar_(
      serie = y,
      name = "# of Youth",
      legend = FALSE
    ) |>
    echarts4r::e_add_nested('extra', pct) |>
    echarts4r::e_tooltip(
      trigger = "axis",
      formatter = htmlwidgets::JS("
        function(params) {
          return(
            params[0].seriesName +
            '<br/>' + params[0].marker + params[0].value[1] +
            '<br/>' + '<strong>' + params[0].data.value[0].toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',') + ' (' + Math.round(params[0].data.extra.pct * 100) + '%)' + '</strong>'
          )
        }")
    ) |> 
    echarts4r::e_grid(containLabel = TRUE)

  if (axis_flip) {
    out <- out |>
      echarts4r::e_flip_coords()
  }

  out
}


#' Generate a Sankey chart using echarts4r
#'
#' This function generates a Sankey chart using the echarts4r package.
#'
#' @param data A data frame containing the data to be plotted.
#' @param entry_status A character string specifying the column name in the data
#' frame representing the entry status of the flow.
#' @param exit_status A character string specifying the column name in the data
#' frame representing the exit status of the flow.
#' @param count A character string specifying the column name in the data frame
#' representing the count or value associated with each flow.
#' @param color A character string specifying the color of the Sankey chart.
#' Default is "blue".
#'
#' @return A Sankey chart visualized using echarts4r.
#'
#' @examples
#' \dontrun{
#' mock_data <- data.frame(
#'   status_at_entry = c("A", "A", "B", "B"),
#'   status_at_exit = c("X", "Y", "Y", "Z"),
#'   n = c(10, 20, 30, 20)
#' )
#' sankey_chart(
#'   data = mock_data,
#'   entry_status = "status_at_entry",
#'   exit_status = "status_at_exit",
#'   count = "n",
#'   color = "green"
#' )
#' }
sankey_chart <- function(data,
                         entry_status,
                         exit_status,
                         count,
                         color = "blue") {

  data |>
    echarts4r::e_charts() |>
    echarts4r::e_sankey_(
      source = entry_status,
      target = exit_status,
      value = count,
      layoutIterations = 0
    ) |>
    echarts4r::e_tooltip(trigger = "item") |>
    echarts4r::e_color(color = color) |>
    echarts4r::e_grid(containLabel = TRUE)

}

#' Prepare data for a Sankey chart
#'
#' `prepare_sankey_data()` prepares data for visualization in a Sankey chart.
#'
#' @param data A data frame
#' @param response_col A character string specifying the name of the column 
#'   with response values
#' @param response_vals A vector of response values to include in the Sankey chart
#'
#' @return A data frame summarizing the transitions between entry and exit 
#'   response values, with columns `Entry`, `Exit`, and `n` representing 
#'   the start, end and count of each transition.
#'
#' @examples
#' \dontrun{
#' mock_data <- data.frame(
#'   enrollment_id = rep(1:5, each = 2),
#'   personal_id = rep(1:5, each = 2),
#'   organization_id = 1,
#'   data_collection_stage = rep(c("Project start", "Project exit"), 5),
#'   status = c("A", "B", "A", "B", "B", "C", "A", "C", "A", "D")
#' )
#' 
#' prepare_sankey_data(mock_data, response_col = "status", response_vals = c("A", "B", "C"))
#' }
prepare_sankey_data <- function(data, response_col, response_vals) {
  data |> 
    dplyr::filter(
      # Each enrollment has one "Project start" and at most one "Project exit"
      data_collection_stage %in% c("Project start", "Project exit"),
      # Not all response values are included in the sankey chart
      !!rlang::sym(response_col) %in% response_vals
    ) |> 
    # Select columns of interest
    dplyr::select(enrollment_id, personal_id, organization_id, data_collection_stage, !!rlang::sym(response_col)) |> 
    # Pivot to one row per youth
    tidyr::pivot_wider(names_from = data_collection_stage, values_from = !!rlang::sym(response_col)) |> 
    # Filter rows with both "Project start" and "Project exit"
    dplyr::filter(!is.na(`Project start`), !is.na(`Project exit`)) |>
    dplyr::count(`Project start`, `Project exit`) |> 
    dplyr::rename("Entry" = "Project start", "Exit" = "Project exit") |> 
    dplyr::mutate(
      Entry = paste0(Entry, " (Entry)"),
      Exit = paste0(Exit, " (Exit)")
    )
}
