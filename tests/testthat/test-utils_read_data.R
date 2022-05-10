
test_that("`get_export_dates()` throws error if nrow != 1", {

  # Create a temp file
  dir <- fs::path_temp()

  # Write the headers to the temp file
  writeLines(
    text = "ExportStartDate, ExportEndDate",
    con = fs::path(dir, "Export.csv")
  )

  headers_only <- export_file

  # Ensure that the file containing only headers (no rows of data) throws an
  # error
  testthat::expect_error(
    get_export_dates(dir = dirname(headers_only)),
    "Expected exactly 1 row of data, but found 0 rows"
  )

})
