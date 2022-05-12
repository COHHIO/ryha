#' read_data
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
read_ethnicity <- function() {

  client <- readr::read_csv(
    file = here::here("data/Client.csv"),
    col_select = c(PersonalID, DOB, AmIndAKNative:GenderNone),
    col_types = readr::cols(
      .default = readr::col_integer(),
      PersonalID = readr::col_character(),
      DOB = readr::col_date()
    )
  ) |>
    dplyr::rename(HispanicLatino = Ethnicity)

  ethnicity <- client |>
    dplyr::select(-DOB) |>
    dplyr::select(PersonalID, AmIndAKNative:HispanicLatino) |>
    tidyr::pivot_longer(
      cols = -PersonalID,
      names_to = "Ethnicity",
      values_to = "Status",
      values_transform = list(Status = as.integer)
    ) |>
    dplyr::filter(Status == 1L) |>
    dplyr::select(-Status)

  gender <- client |>
    dplyr::select(-DOB) |>
    dplyr::select(PersonalID, Female:Questioning) |>
    tidyr::pivot_longer(
      cols = -PersonalID,
      names_to = "Gender",
      values_to = "Status",
      values_transform = list(Status = as.integer)
    ) |>
    dplyr::filter(Status == 1L) |>
    dplyr::select(-Status)

  list(
    client_full = client,
    ethnicity = ethnicity,
    gender = gender
  )

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



# Master function for reading in all .csv data and converting to .parquet files
get_export_dates <- function(dir) {

  # Ensure there is a file in the directory titled "Export.csv"
  export_file_exists <- fs::file_exists(
    path = fs::path(dir, "Export.csv")
  )

  if (!export_file_exists) {

    rlang::abort("Could not find `Export.csv` file in directory")

  }

  # Make sure that the "ExportStartDate" and "ExportEndDate" columns exist in
  # the .csv
  header <- readLines(
    con = fs::path(dir, "Export.csv"),
    n = 1
  )

  col_names_check <- c(
    stringr::str_detect(
      string = header,
      pattern = "ExportStartDate",
      negate = TRUE
    ),
    stringr::str_detect(
      string = header,
      pattern = "ExportEndDate",
      negate = TRUE
    )
  )

  if (any(col_names_check)) {

    paste0(
      "Could not find ",
      ifelse(col_names_check[1], "`ExportStartDate` "),
      ifelse(all(col_names_check), "and "),
      ifelse(col_names_check[2], "`ExportEndDate` "),
      "columns in the `Export.csv` file"
    ) |>
      rlang::abort()

  }

  # Get the date range of the export
  export_data <- readr::read_csv(
    file = fs::path(dir, "Export.csv"),
    col_select = c(ExportStartDate, ExportEndDate),
    col_types = readr::cols(
      .default = readr::col_date(format = "%m/%d/%Y")
    )
  )

  # Ensure that neither of the two dates are NA values
  check_nas <- c(
    export_data$ExportStartDate[1],
    export_data$ExportEndDate[1]
  ) |>
    is.na()

  if (any(check_nas)) {

    paste0(
      "A valid ",
      ifelse(check_nas[1], "`ExportStartDate` ", ""),
      ifelse(all(check_nas), "and ", ""),
      ifelse(check_nas[2], "`ExportEndDate` ", ""),
      "could not be found in `Export.csv`"
    ) |>
      rlang::abort()

  }

  num_rows <- nrow(export_data)

  # Ensure that there was exactly one row
  if (num_rows != 1L) {

    glue::glue("Expected exactly 1 row of data, but found {num_rows} rows.") |>
      rlang::abort()

  }

  list(
    export_start_date = export_data$ExportStartDate[1],
    export_end_date = export_data$ExportEndDate[1]
  )

}
