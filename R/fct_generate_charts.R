


pie_chart <- function(data, category, count) {

  data |>
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



bar_chart <- function(data, x, y, axis_flip = TRUE) {

  out <- data |>
    echarts4r::e_charts_(x = x) |>
    echarts4r::e_bar_(
      serie = y,
      name = "# of Youth",
      legend = FALSE
      # label = list(
      #   formatter = '{@[0]}',
      #   show = TRUE,
      #   position = "right"
      # )
    ) |>
    echarts4r::e_tooltip(trigger = "item") |>
    echarts4r::e_grid(containLabel = TRUE)

  if (axis_flip) {

    out <- out |>
      echarts4r::e_flip_coords()

  }

  out

}



sankey_chart <- function(data, entry_status, exit_status, count,
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


get_ids_for_sankey <- function(data) {

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


