


#' Process HMIS Data
#'
#' @details This is the *"master"* function that governs the entire ETL process
#'   for processing the uploaded data and, depending on the user (i.e., whether or
#'   not the user is an approved grantee), writing the data out to the DropBox
#'   data lake
#'
#' @param file String, the full path to the .zip file containing the quarterly
#'   HMIS data
#'
#' @return
#' @export
#'
#' @examples
process_data <- function(file) {

  # Ensure that the uploaded file is indeed a .zip file
  # ...

  # Create a temporary directory to unzip the .csv files into
  tmp_dir <- tempfile()

  # Ensure that this is a new (i.e., empty) directory
  if (fs::dir_exists(tmp_dir)) {

    fs::dir_delete(tmp_dir)

  }

  # Unzip the .csv files into the temp directory
  zip::unzip(
    zipfile = zip_file,
    exdir = tmp_dir,
    overwrite = TRUE   # overwrite any previous uploads into same temp directory
  )

  # Check that all required HMIS files are present in uploaded .zip file
  check <- check_file_names(dir = tmp_dir)

  # Run more checks...



  # Process "Client" file
  client_file_path <- tmp_dir |>
    stringr::str_subset("Client.csv$")

  # Get the data from the individual .csv file

  # Perform whatever necessary ETL we need to do on it (joining to lookup table,
  # specifying columns, pivoting, etc.)

  # Creating `.parquet` table that's ready to be written out to data lake

  # Here's the function that governs
  # process_client()



  # Move to Dropbox data lake



  # TODO // Decide if we need to do this last...
  # I imagine we *might* still need a temp directory if the user is a non-
  # grantee (i.e., we aren't storing their data in the Dropbox data lake).
  # If the user *is* a grantee and we are storing their data in the data lake,
  # then we should be able to delete this temp directory without issue.
  fs::dir_delete(tmp_dir)

  # Generate "Submission" data


}




generate_submission_id <- function(dir, contact_first_name, contact_last_name,
                                   contact_email, program_id,
                                   period_start_date, period_end_date) {

  # Handle "first" submission, to generate ID 1, when there is no historical
  # "submission" file in the data lake
  existing_sub_files <- fs::dir_info(
    path = dir,
    recurse = TRUE,
    type = "file"
  )

  if (nrow(existing_sub_files) == 0) {

    submission_id <- 1L

  } else {

    max_sub_id <- arrow::read_parquet(
      file = "some_file",
      col_select = SubmissionID
    ) |>
      dplyr::filter(SubmissionID == max(SubmissionID)) |>
      dplyr::collect() |>
      dplyr::pull(SubmissionID)

    # Stop if there isn't exactly 1 max SubmissionID
    if (length(max_sub_id) != 1L) {

      rlang::abort(
        "Found the same `max` SubmissionID in the `Submission` data lake table"
      )

    }

    submission_id <- max_sub_id + 1L

  }

  # TODO // Figure out how to retrieve "ProgramID" from `Program` data lake table

  sub_data <- tibble::tibble(
    SubmissionID = submission_id,
    DateTimeSubmitted = Sys.time(),
    DateSubmitted = Sys.Date(),
    SourceContactFirstName = contact_first_name,
    SourceContactLastName = contact_last_name,
    SourceContactEmailAddress = contact_email,
    ProgramID = program_id,
    PeriodDateStart = period_start_date,
    PeriodDateEnd = period_end_date,
  )


  last_submission <- arrow::read_parquet(
    file = file,
    col_select = c(SubmissionID, DatetimeSubmitted)
  ) |>
    dplyr::filter(SubmissionID == max(SubmissionID)) |>
    dplyr::collect()

  last_submission_id <- last_submission |>
    dplyr::pull(SubmissionID)

  if (length(last_submission_id) != 1L) {

    if (length(last_submission_id) == 0) {

      rlang::inform("First submission")

    }

    rlang::abort("Expected ")

  }

}




# TODO // Since we are handling ingest into an R data frame elsewhere, the
# function argument here should be 'data' instead of a directory...
# I think we can assume that the data was read into an R data frame successfully

#' Retrieve Export Dates
#'
#' @param dir String, the location of the
#'
#' @return
#' @export
#'
#' @examples
get_export_dates <- function(dir) {

  # Make sure that the "ExportStartDate" and "ExportEndDate" columns exist in
  # the .csv
  header <- readLines(
    con = fs::path(dir, "Export.csv"),
    n = 1
  )

  # Check to see if the "ExportStartDate" and "ExportEndDate" columns exist in
  # the data
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

  # Stop if column names are not found
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

  # Return the start and end dates of the exported data
  list(
    export_start_date = export_data$ExportStartDate[1],
    export_end_date = export_data$ExportEndDate[1]
  )

}

