#' read_data
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
read_client <- function() {

  full <- readr::read_csv(
    file = here::here("data/Client.csv"),
    col_select = c(
      PersonalID, SSN, SSNDataQuality, DOB, DOBDataQuality,
      AmIndAKNative:DischargeStatus
    ),
    col_types = readr::cols(
      .default = readr::col_integer(),
      PersonalID = readr::col_character(),
      SSN = readr::col_character(),
      DOB = readr::col_date()
    )
  ) |>
    dplyr::rename(HispanicLatino = Ethnicity)

  client <- full |>
    dplyr::select(PersonalID, SSN, SSNDataQuality, DOB, DOBDataQuality)

  ethnicity <- full |>
    dplyr::select(-c(SSN, SSNDataQuality, DOB, DOBDataQuality)) |>
    dplyr::select(PersonalID, AmIndAKNative:HispanicLatino) |>
    tidyr::pivot_longer(
      cols = -PersonalID,
      names_to = "Ethnicity",
      values_to = "Status",
      values_transform = list(Status = as.integer)
    ) |>
    dplyr::filter(Status == 1L) |>
    dplyr::select(-Status)

  gender <- full |>
    dplyr::select(-c(SSN, SSNDataQuality, DOB, DOBDataQuality)) |>
    dplyr::select(PersonalID, Female:Questioning) |>
    tidyr::pivot_longer(
      cols = -PersonalID,
      names_to = "Gender",
      values_to = "Status",
      values_transform = list(Status = as.integer)
    ) |>
    dplyr::filter(Status == 1L) |>
    dplyr::select(-Status)

  military <- full |>
    dplyr::select(PersonalID, VeteranStatus:DischargeStatus)

  list(
    client = client,
    ethnicity = ethnicity,
    gender = gender,
    military = military
  )

}



process_ethnicity <- function(client_data, submission_id) {

  ethnicity <- client_data |>
    dplyr::select(-DOB) |>
    dplyr::select(PersonalID, AmIndAKNative:HispanicLatino) |>
    tidyr::pivot_longer(
      cols = -PersonalID,
      names_to = "Ethnicity",
      values_to = "Status",
      values_transform = list(Status = as.integer)
    ) |>
    dplyr::filter(Status == 1L) |>
    dplyr::select(-Status) |>
    dplyr::mutate(SubmissionID = submission_id) |>
    dplyr::select(SubmissionID, dplyr::everything())

  ethnicity

}



#' Title
#'
#' @param dir (String) Path to the directory containing the "Submission"
#'   .parquet file(s) in the data lake
#'
#' @return
#' @export
#'
#' @examples
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





#' Title
#'
#' @param zip_file
#'
#' @return
#' @export
#'
#' @examples
process_data <- function(zip_file) {

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

  # Move to Dropbox data lake



  # TODO // Decide if we need to do this last...
  # I imagine we *might* still need a temp directory if the user is a non-
  # grantee (i.e., we aren't storing their data in the Dropbox data lake).
  # If the user *is* a grantee and we are storing their data in the data lake,
  # then we should be able to delete this temp directory without issue.
  fs::dir_delete(tmp_dir)

  # Generate "Submission" data


}


# Helper function that converts a character vector to an HTML bullet-point list
vec_to_ul <- function(vec) {

  # Create the individual bullet-points
  bullets <- vec |>
    purrr::map(.f = function(x) shiny::tags$li(x))

  # Return the un-ordered list ("ul") containing the individual bullet-points
  shiny::tagList(
    shiny::tags$ul(
      bullets
    )
  )

}


# Compare the file names within the uploaded .zip file to the expected file
# names (stored in the 'HMISmetadata' data object within this R package)
check_file_names <- function(dir) {

  # Retrieve the full paths to each individual file extracted from the .zip file
  paths <- fs::dir_info(dir) |>
    dplyr::pull(path)

  # Retrieve the related directory
  dir <- dirname(paths) |> unique()

  # Remove the directory from the path, so that we are just left with the file
  # names themselves (e.g., "path/to/data.csv" --> "data.csv")
  file_names <- paths |>
    stringr::str_replace(
      pattern = paste0(dir, "/"),
      replacement = ""
    )

  # Assume that there were no discrepancies in the uploaded file names
  valid <- TRUE

  # Compare the character vector of file names from the .zip file to the
  # file names we expect based on the 'HMISmetadata' data in this R package
  missing_from_upload <- setdiff(
    x = HMISmetadata$FileName,
    y = file_names
  )

  # If at least 1 file was found to be missing from the uploaded .zip file...
  if (length(missing_from_upload) > 0) {

    # ... set the "valid" flag to FALSE
    valid <- FALSE

  }

  # Return the "valid" flag and accompanying message (if applicable)
  out <- list(
    valid = valid,
    missing_file_names = missing_from_upload
  )

  return(out)

}

