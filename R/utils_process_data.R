


#' Process HMIS Data
#'
#' @details This is the *"master"* function that governs the entire ETL process
#'   for processing the uploaded data and, depending on the user (i.e., whether
#'   or not the user is an approved grantee), writing the data out to the
#'   PostgreSQL database.
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
  is_zip <- stringr::str_detect(
    string = file,
    pattern = ".zip$"
  )

  if (!is_zip) {

    paste0(file, " must be a .zip file") |>
      rlang::abort()

  }

  # Create a temporary directory to unzip the .csv files into
  tmp_dir <- tempfile()

  # Ensure that this is a new (i.e., empty) directory
  if (fs::dir_exists(tmp_dir)) {

    fs::dir_delete(tmp_dir)

  }

  # Unzip the .csv files into the temp directory
  zip::unzip(
    zipfile = file,
    exdir = tmp_dir,
    overwrite = TRUE   # overwrite any previous uploads into same temp directory
  )

  # Check that all required HMIS files are present in uploaded .zip file
  check <- check_file_names(dir = tmp_dir)

  # List the files (full paths) in the temp directory
  files_in_tmp <- fs::dir_ls(tmp_dir)

  #
  files_list <- list(
    client = stringr::str_subset(string = files_in_tmp, pattern = "Client.csv$"),
    disabilities = stringr::str_subset(string = files_in_tmp, pattern = "Disabilities.csv$"),
    education = stringr::str_subset(string = files_in_tmp, pattern = "EmploymentEducation.csv$"),
    employment = stringr::str_subset(string = files_in_tmp, pattern = "EmploymentEducation.csv$"),
    living = stringr::str_subset(string = files_in_tmp, pattern = "CurrentLivingSituation.csv$"),
    health = stringr::str_subset(string = files_in_tmp, pattern = "HealthAndDV.csv$"),
    domestic_violence = stringr::str_subset(string = files_in_tmp, pattern = "HealthAndDV.csv$"),
    income = stringr::str_subset(string = files_in_tmp, pattern = "IncomeBenefits.csv$"),
    benefits = stringr::str_subset(string = files_in_tmp, pattern = "IncomeBenefits.csv$"),
    enrollment = stringr::str_subset(string = files_in_tmp, pattern = "Enrollment.csv$"),
    services = stringr::str_subset(string = files_in_tmp, pattern = "Services.csv$"),
    project = stringr::str_subset(string = files_in_tmp, pattern = "Project.csv$"),
    organization = stringr::str_subset(string = files_in_tmp, pattern = "Organization.csv$"),
    exit = stringr::str_subset(string = files_in_tmp, pattern = "Exit.csv$"),
    export = stringr::str_subset(string = files_in_tmp, pattern = "Export.csv$")
  )

  funcs_list <- list(
    client = read_client,
    disabilities = read_disabilities,
    education = read_education,
    employment = read_employment,
    living = read_living,
    health = read_health,
    domestic_violence = read_domestic_violence,
    income = read_income,
    benefits = read_benefits,
    enrollment = read_enrollment,
    services = read_services,
    project = read_project,
    organization = read_organization,
    exit = read_exit,
    export = read_export
  )

  # This loop ingest each file with its respected `read_*()` function, and
  # stores each data frame in a list
  data <- list(
    client = NULL,
    disabilities = NULL,
    education = NULL,
    employment = NULL,
    living = NULL,
    health = NULL,
    domestic_violence = NULL,
    income = NULL,
    benefits = NULL,
    enrollment = NULL,
    services = NULL,
    project = NULL,
    organization = NULL,
    exit = NULL,
    export = NULL
  )


  # Read in each file one at a time, creating a list of data frames for each
  # table; this loop is an all-or-nothing approach, the `data` object will
  # either include all of the data frames (if successful) or the first
  # warning/error message encountered
  for (i in 1:length(files_list)) {

    temp <- tryCatch(
      {
        rlang::exec(funcs_list[[i]], files_list[[i]])
      },
      error = function(cond) {
        out <- glue::glue(
          "
          There was an error in processing the \"{basename(files_list[[i]])}\" file.
          Please check your file export settings, re-download the .zip folder, and try again.
          "
        )
        return(out)
      },
      warning = function(cond) {
        out <- glue::glue(
          "
          There was a warning in processing the \"{basename(files_list[[i]])}\" file.
          Please check your file export settings, re-download the .zip folder, and try again.
          This is most likely the result of incorrectly formatted dates.
          "
        )
        return(out)
      }
    )

    if (!"data.frame" %in% class(temp)) {

      data <- temp

      {break}

    } else {

      data[[i]] <- temp

    }

  }

  return(data)

}



prep_tables <- function(data, conn) {

  file_data <- data$project |>
    dplyr::rename(orig_project_id = project_id) |>
    dplyr::left_join(
      data$organization |> dplyr::select(organization_id, organization_name),
      by = "organization_id"
    ) |>
    dplyr::mutate(
      software_name = data$export$software_name[1]
    )

  if ("project" %in% DBI::dbListTables(conn = conn)) {

    res <- DBI::dbSendQuery(
      conn = conn,
      statement = "
      SELECT
        project_id,
        project_name,
        orig_project_id,
        organization_name,
        software_name
      FROM project
      ORDER BY project_id
    "
    )
    db_data <- DBI::dbFetch(res)
    DBI::dbClearResult(res)

    file_data <- file_data |>
      dplyr::left_join(
        db_data,
        by = c(
          "orig_project_id",
          "project_name",
          "organization_name",
          "software_name"
        )
      )

    if (any(is.na(file_data$project_id))) {

      max_current_project_id <- max(db_data$project_id)

      file_data_existing_projects <- file_data |>
        dplyr::filter(!is.na(project_id))

      file_data_new_projects <- file_data |>
        dplyr::filter(is.na(project_id)) |>
        dplyr::mutate(project_id = max_current_project_id + dplyr::row_number())

      # Append new projects to "project" database table
      DBI::dbWriteTable(
        conn = conn,
        name = "project",
        value = file_data_new_projects,
        append = TRUE
      )

      file_data <- file_data_existing_projects |>
        dplyr::bind_rows(file_data_new_projects)

    }

  } else {

    file_data_new_projects <- file_data |>
      dplyr::mutate(project_id = dplyr::row_number())

    # Append new projects to "project" database table
    DBI::dbWriteTable(
      conn = conn,
      name = "project",
      value = file_data_new_projects,
      append = TRUE
    )

    file_data <- file_data_new_projects

  }

  # Get current "submission_id" value
  if ("submission" %in% DBI::dbListTables(conn = conn)) {

    res <- DBI::dbSendQuery(
      conn = conn,
      statement = "
      SELECT
        MAX(submission_id) as max_submission_id
      FROM submission
    "
    )
    db_data <- DBI::dbFetch(res)
    DBI::dbClearResult(res)

    current_submission_id <- db_data$max_submission_id + 1L

  } else {

    current_submission_id <- 1L

  }

  # https://stackoverflow.com/questions/53720531/postgres-array-column-type-to-tbl-list-column-in-r-and-viceversa
  # Create submission table
  data$submission <- data$export |>
    dplyr::mutate(
      submission_id = current_submission_id,
      date_time_submitted = Sys.time(),
      date_submitted = Sys.Date(),
      project_id = file_data$project_id |>
        unique() |>
        sort() |>
        paste(collapse = ", ")
    )

  # Add project_id to "enrollment" file data
  data$enrollment <- data$enrollment |>
    dplyr::left_join(
      file_data |> dplyr::select(project_id, software_name, orig_project_id),
      by = c("orig_project_id")
    ) |>
    dplyr::mutate(
      submission_id = current_submission_id
    )

  data$client <- data$client |>
    dplyr::mutate(
      software_name = data$export$software_name[1],
      submission_id = current_submission_id
    )

  data$disabilities <- data$disabilities |>
    dplyr::mutate(
      software_name = data$export$software_name[1],
      submission_id = current_submission_id
    )

  data$education <- data$education |>
    dplyr::mutate(
      software_name = data$export$software_name[1],
      submission_id = current_submission_id
    )

  data$employment <- data$employment |>
    dplyr::mutate(
      software_name = data$export$software_name[1],
      submission_id = current_submission_id
    )

  data$living <- data$living |>
    dplyr::mutate(
      software_name = data$export$software_name[1],
      submission_id = current_submission_id
    )

  data$health <- data$health |>
    dplyr::mutate(
      software_name = data$export$software_name[1],
      submission_id = current_submission_id
    )

  data$domestic_violence <- data$domestic_violence |>
    dplyr::mutate(
      software_name = data$export$software_name[1],
      submission_id = current_submission_id
    )

  data$income <- data$income |>
    dplyr::mutate(
      software_name = data$export$software_name[1],
      submission_id = current_submission_id
    )

  data$benefits <- data$benefits |>
    dplyr::mutate(
      software_name = data$export$software_name[1],
      submission_id = current_submission_id
    )

  data$services <- data$services |>
    dplyr::mutate(
      software_name = data$export$software_name[1],
      submission_id = current_submission_id
    )

  data$exit <- data$exit |>
    dplyr::mutate(
      software_name = data$export$software_name[1],
      submission_id = current_submission_id
    )

  out <- c(
    "submission",
    "client",
    "enrollment",
    "disabilities",
    "education",
    "employment",
    "living",
    "health",
    "domestic_violence",
    "income",
    "benefits",
    "services",
    "exit"
  )

  return(data[out])

}



send_to_db <- function(data, conn) {

  for (i in 1:length(data)) {

    table_name <- names(data)[i]

    DBI::dbWriteTable(
      conn = conn,
      table_name,
      data[[table_name]],
      append = TRUE
    )

    # Give the PostgreSQL database a second to breathe between writes
    Sys.sleep(0.5)

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

