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
#' mock_data <- data.frame(x = c("A", "B", "C"), y = c(10, 20, 30))
#' pie_chart(
#'   data = mock_data,
#'   category = "x",
#'   count = "y"
#' )
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
#' @param axis_flip A logical value indicating whether to flip the x and y axes.
#' Default is TRUE.
#'
#' @return A bar chart visualized using echarts4r.
#'
#' @examples
#' mock_data <- data.frame(x = c("A", "B", "C"), y = c(10, 20, 30))
#' bar_chart(
#'   data = mock_data,
#'   x = "x",
#'   y = "y"
#' )
bar_chart <- function(data, x, y, axis_flip = TRUE) {

  out <- data |>
    echarts4r::e_charts_(x = x) |>
    echarts4r::e_bar_(
      serie = y,
      name = "# of Youth",
      legend = FALSE
    ) |>
    echarts4r::e_tooltip(trigger = "item") |>
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

#' Get eligible IDs for generating a Sankey chart
#'
#' `get_ids_for_sankey()` identifies IDs with both a Project start and a
#' Project exit data collection stage.
#'
#' @details
#' For entries with multiple Project start the most recently updated data is kept.
#'
#' @param data A data frame containing the data from which to extract unique IDs.
#'
#' @return A data frame with unique combinations of organization and personal IDs
#' that have at least a Project start and Project exit data collection stage.
#'
#' @examples
#' mock_data <- tibble::tribble(
#'   ~organization_id, ~personal_id, ~data_collection_stage, ~date_updated,
#'   1, 101, "Project start", "2023-01-01",
#'   1, 101, "Project exit", "2023-12-31",
#'   2, 102, "Project start", "2023-01-01",
#'   2, 102, "Project exit", "2023-12-31",
#'   2, 103, "Project start", "2023-02-01",
#'   3, 104, "Project start", "2023-02-01",
#'   3, 104, "Project update", "2023-12-15",
#'   3, 105, "Project start", "2023-01-01",
#'   3, 105, "Project update", "2023-06-30",
#'   3, 105, "Project exit", "2023-12-31"
#' )
#' get_ids_for_sankey(mock_data)
get_ids_for_sankey <- function(data) {

  # TODO: Check scenario where a person has start-end-start.
  data |>
    dplyr::filter(
      data_collection_stage %in% c("Project start", "Project exit")
    ) |>
    dplyr::arrange(
      organization_id,
      personal_id,
      data_collection_stage,
      # we'll keep only the most recently updated data
      dplyr::desc(date_updated)
    ) |>
    dplyr::distinct(
      organization_id,
      personal_id,
      data_collection_stage
    ) |>
    dplyr::group_by(organization_id, personal_id) |>
    # ensure there's exactly 2 rows of data by individual (an entry & an exit)
    dplyr::filter(dplyr::n() == 2L) |>
    dplyr::ungroup() |>
    dplyr::distinct(organization_id, personal_id)

}

#' Prepare data for generating a Sankey chart
#'
#' `prep_sankey_data()` prepares data by filtering, arranging, selecting, and
#' transforming it to generate data suitable for creating a Sankey chart
#' representing flow between stages.
#'
#' @param data A data frame containing the data to be prepared.
#' @param state_var A character string specifying the column name representing
#' the state variable to be used in the Sankey chart.
#'
#' @return A data frame with prepared data suitable for generating a Sankey chart.
#'
#' @examples
#' mock_data <- tibble::tribble(
#'   ~organization_id, ~personal_id, ~data_collection_stage, ~date_updated, ~condition,
#'   1, 101, "Project start", "2023-01-01", "A",
#'   1, 101, "Project exit", "2023-12-31", "A",
#'   2, 102, "Project start", "2023-01-01", "A",
#'   2, 102, "Project exit", "2023-12-31", "B",
#'   2, 103, "Project start", "2023-02-01", "A",
#'   3, 104, "Project start", "2023-02-01", "B",
#'   3, 104, "Project update", "2023-12-15", "B",
#'   3, 105, "Project start", "2023-01-01", "B",
#'   3, 105, "Project update", "2023-06-30", "B",
#'   3, 105, "Project exit", "2023-12-31", "A"
#' )
#' prep_sankey_data(data = mock_data, state_var = "condition")
prep_sankey_data <- function(data, state_var) {

  data |>
    dplyr::filter(
      data_collection_stage %in% c("Project start", "Project exit")
    ) |>
    dplyr::arrange(
      organization_id,
      personal_id,
      data_collection_stage,
      # keep only the most recently updated data
      dplyr::desc(date_updated)
    ) |>
    dplyr::select(
      organization_id,
      personal_id,
      data_collection_stage,
      {{ state_var }}
    ) |>
    dplyr::distinct(
      organization_id,
      personal_id,
      data_collection_stage,
      .keep_all = TRUE
    ) |>
    dplyr::mutate(
      id = paste0(organization_id, "_", personal_id)
    ) |>
    dplyr::select(id, data_collection_stage, {{ state_var }}) |>
    dplyr::mutate(
      data_collection_stage = dplyr::case_when(
        data_collection_stage == "Project start" ~ "Entry",
        data_collection_stage == "Project exit" ~ "Exit"
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = id,
      names_from = data_collection_stage,
      values_from = {{ state_var }}
    ) |>
    dplyr::mutate(
      Entry = paste0(Entry, " (Entry)"),
      Exit = paste0(Exit, " (Exit)")
    ) |>
    dplyr::count(Entry, Exit)

}
