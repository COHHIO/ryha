
test_that("`get_export_dates()` throws error if nrow != 1", {

  # Create a temp file
  dir <- fs::path_temp()

  file_path <- fs::path(dir, "Export.csv")

  if (fs::file_exists(file_path)) {

    fs::file_delete(file_path)

  }

  # Write the headers to the temp file, but no data
  writeLines(
    text = "ExportStartDate, ExportEndDate",
    con = file_path
  )

  # read.csv(file_path)

  # Ensure that the file containing only headers (no rows of data) throws an
  # error
  testthat::expect_error(
    get_export_dates(dir = dir),
    "A valid `ExportStartDate` and `ExportEndDate` could not be found in `Export.csv`"
  )

  # Add a data point just for "ExportEndDate"
  write(
    x = ", 12/31/2021",
    file = file_path,
    append = TRUE
  )

  # read.csv(file_path)

  testthat::expect_error(
    get_export_dates(dir = dir),
    "A valid `ExportStartDate` could not be found in `Export.csv`"
  )

  fs::file_delete(file_path)

  # Write the headers to the temp file, but no data
  df_two_rows <- tibble::tribble(
    ~ExportStartDate, ~ExportEndDate,
    "1/1/2021", "3/31/2021",
    "4/1/2021", "6/30/2021"
  )

  readr::write_csv(
    x = df_two_rows,
    file = file_path
  )

  # read.csv(file_path)

  testthat::expect_error(
    get_export_dates(dir = dir),
    "Expected exactly 1 row of data, but found 2 rows"
  )

  # Returns list when successful
  df_one_row <- df_two_rows |>
    dplyr::slice(1)

  readr::write_csv(
    x = df_one_row,
    file = file_path,
    append = FALSE   # overwrite existing file
  )

  # read.csv(file_path)

  testthat::expect_type(
    get_export_dates(dir = dir),
    "list"
  )

})
