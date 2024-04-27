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
#' @return A list containing the ingested data for each HMIS table.
#'
#' @export
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
  check <- check_file_names(dir = tmp_dir, metadata = HMISmetadata)

  # Throw error if any needed files are missing
  if (!check$valid) {

    glue::glue(
      "The selected <strong>.zip</strong> is missing the following expected <strong>.csv</strong> file(s):",
      paste("-", check$missing_file_names, collapse = ",<br>"),
      .sep = "<br>"
    ) |>
    rlang::abort()

  }

  # List the files (full paths) in the temp directory
  files_in_tmp <- fs::dir_ls(tmp_dir)

  # Read data from each file
  data <- list(
    client = read_client(find_file(files_in_tmp, "Client")),
    disabilities = read_disabilities(find_file(files_in_tmp, "Disabilities")),
    education = read_education(find_file(files_in_tmp, "EmploymentEducation")),
    employment = read_employment(find_file(files_in_tmp, "EmploymentEducation")),
    living = read_living(find_file(files_in_tmp, "CurrentLivingSituation")),
    health = read_health(find_file(files_in_tmp, "HealthAndDV")),
    domestic_violence = read_domestic_violence(find_file(files_in_tmp, "HealthAndDV")),
    income = read_income(find_file(files_in_tmp, "IncomeBenefits")),
    benefits = read_benefits(find_file(files_in_tmp, "IncomeBenefits")),
    enrollment = read_enrollment(find_file(files_in_tmp, "Enrollment")),
    services = read_services(find_file(files_in_tmp, "Services")),
    project = read_project(find_file(files_in_tmp, "Project")),
    organization = read_organization(find_file(files_in_tmp, "Organization")),
    exit = read_exit(find_file(files_in_tmp, "Exit")),
    export = read_export(find_file(files_in_tmp, "Export"))
  )

  return(data)

}

#' Find the complete filepath of a certain .csv file
#'
#' `find_file()` finds the complete filepath of a .csv file based on the .csv
#' basename (i.e. the file name) in a given vector of filepaths.
#'
#' @param files Character vector containing the filepaths to search through.
#' @param target String specifying the name of the .csv file (without the extension)
#'
#' @return A character vector containing the filepath for the corresponding .csv file.
#'
#' @examples
#' \dontrun{
#' files <- c("some/path/to/file/data1.csv", "some/path/to/file/data2.csv")
#' find_file(files, "data1")
#' }
find_file <- function(files, target) {

  stringr::str_subset(
    string = files,
    pattern = paste0(target, ".csv$")
  )

}

#' Prepare data for database tasks
#'
#' `prep_tables()` processes the list returned by `process_data()` and returns
#' a new list which is used by `delete_from_db()` and `send_to_db()`.
#'
#' @details
#' `prep_tables()` handles the addition of new organization and project entries to
#' the database, adds `project_id` and `organization_id` columns to `enrollment`
#' file data, adds `organization_id` to the remaining files (e.g. client,
#' disabilities, education, ...) and returns them as a list of dataframes.
#'
#' `organization` and `project` dataframes are excluded from the returned list.
#'
#' @param data List of dataframes returned by `process_data()`.
#' @param conn A database connection object.
#'
#' @return A list of processed data frames.
prep_tables <- function(data, conn) {

  # Retrieve the information from the file for the "organization" table
  if (nrow(data$organization) != 1L) {

    glue::glue(
      "{nrow(data$organization)} organizations were found; ",
      "expected exactly 1 organization in .zip upload."
    ) |>
      rlang::abort()

  }

  # If the "organization" table exists in the database...
  if ("organization" %in% DBI::dbListTables(conn = conn)) {

    # Retrieve the existing organization information from the database
    res <- DBI::dbSendQuery(
      conn = conn,
      statement = "
      SELECT
        organization_id,
        orig_organization_id,
        organization_name
      FROM organization
      ORDER BY organization_id
    "
    )
    db_data <- DBI::dbFetch(res)
    DBI::dbClearResult(res)

    # Join the database data to the file data; this will allow us to quickly
    # identify which organizations in the uploaded file are not yet in the
    # database
    data$organization <- data$organization |>
      dplyr::left_join(
        db_data,
        by = c(
          "orig_organization_id",
          "organization_name"
        )
      )

    # If the organization in the file doesn't exist in the database...
    if (is.na(data$organization$organization_id[1])) {

      # Determine the max "organization_id" integer value in the database
      max_current_organization_id <- ifelse(
        nrow(db_data) >= 1L,
        max(db_data$organization_id),
        0L
      )

      # Set the next "organization_id" value
      data$organization$organization_id[1] <- max_current_organization_id + 1L

      # Append new organizations to "organization" database table
      DBI::dbWriteTable(
        conn = conn,
        name = "organization",
        value = data$organization,
        append = TRUE
      )

    }

    # If the "organization" table does *not* yet exist in the database...
    # (this handles the first ever submission, if the table hasn't been set up)
  } else {

    # Set the "organization_id" value as 1
    data$organization$organization_id <- 1L

    # Create the "organization" database table
    DBI::dbWriteTable(
      conn = conn,
      name = "organization",
      value = data$organization
    )

  }

  # Retrieve the project, organization, and software information from the
  # uploaded data
  file_data <- data$project |>
    dplyr::rename(orig_project_id = project_id) |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  # If the "project" table exists in the database...
  if ("project" %in% DBI::dbListTables(conn = conn)) {

    # Retrieve the existing project, organization, and software information from
    # the database
    res <- DBI::dbSendQuery(
      conn = conn,
      statement = "
      SELECT
        project_id,
        project_name,
        orig_project_id,
        organization_id
      FROM project
      ORDER BY project_id
    "
    )
    db_data <- DBI::dbFetch(res)
    DBI::dbClearResult(res)

    # Join the database data to the file data; this will allow us to quickly
    # identify which projects in the uploaded file are not yet in the database
    file_data <- file_data |>
      dplyr::left_join(
        db_data,
        by = c(
          "project_name",
          "orig_project_id",
          "organization_id"
        )
      )

    # If there are any "new" projects in the file that don't exist in the
    # database...
    if (any(is.na(file_data$project_id))) {

      # Determine the max "project_id" integer value in the database
      max_current_project_id <- ifelse(
        nrow(db_data) >= 1L,
        max(db_data$project_id),
        0L
      )

      # Isolate the projects in the file that already exist in the database
      file_data_existing_projects <- file_data |>
        dplyr::filter(!is.na(project_id))

      # Isolate the "new" projects in the file that *don't* exist in the
      # database, and define the `project_id` values for each new project
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

      # Append the new projects (and their newly-defined `project_id` values) to
      # existing projects
      file_data <- file_data_existing_projects |>
        dplyr::bind_rows(file_data_new_projects)

    }

    # If the "project" table does *not* yet exist in the database...
    # (this handles the first ever submission, if the table hasn't been set up)
  } else {

    # Set the "project_id" value for each project in the uploaded file
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

  # Add `project_id` and 'organization_id' columns to "enrollment" file data
  data$enrollment <- data$enrollment |>
    dplyr::left_join(
      file_data |> dplyr::select(project_id, orig_project_id, organization_id),
      by = c("orig_project_id")
    )

  # Add `software_name` to remaining files
  data$client <- data$client |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  data$disabilities <- data$disabilities |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  data$education <- data$education |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  data$employment <- data$employment |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  data$living <- data$living |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  data$health <- data$health |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  data$domestic_violence <- data$domestic_violence |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  data$income <- data$income |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  data$benefits <- data$benefits |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  data$services <- data$services |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  data$exit <- data$exit |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  data$export <- data$export |>
    dplyr::mutate(
      organization_id = data$organization$organization_id[1]
    )

  out <- c(
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
    "exit",
    "export"
  )

  return(data[out])

}

#' Delete records from a database
#'
#' `delete_from_db()` deletes records from a database based on uploaded data.
#'
#' @details
#' `delete_from_db()` iterates through each table in the database (except for
#' 'organization' and 'project') and deletes records that match on the table's
#' `*_id` value and `organization_id` value, when compared to the respective
#' uploaded file data.
#'
#' @param data List of dataframes returned by `prep_tables()`.
#' @param conn A database connection object.
#'
#' @return Nothing. `delete_from_db()` is called for its side effects.
delete_from_db <- function(data, conn) {

  for (i in 1:length(data)) {

    # Ensure that a valid 'organization_id' value exists in the input `data`
    # to use in the DELETE statement's WHERE clause, and a valid database table
    if (nrow(data[[i]]) >= 1L & names(data)[i] %in% DBI::dbListTables(conn = conn)) {

      table_name <- glue::glue_sql(
        names(data)[i],
        .con = conn
      )

      # TODO: Should we add an additional check to ensure that table_name is not
      # "organization" nor "project"? Right now we are assuming that prep_tables()
      # is never going to return these data frames

      organization_id <- data[[i]] |>
        dplyr::slice(1) |>
        dplyr::pull(organization_id) |>
        glue::glue_sql(.con = conn)

      sql_stmt <- glue::glue_sql(
      "
      DELETE FROM {table_name}
      WHERE organization_id = {organization_id}
      ",
        .con = conn
      )

      DBI::dbExecute(
        conn = conn,
        statement = sql_stmt
      )

    }

  }

}

#' Send data to a database
#'
#' `send_to_db()` appends each data frame in a list to the corresponding table
#' in a database.
#'
#' @param data List of dataframes returned by `prep_tables()`.
#' @param conn A database connection object.
#' @param waiter An optional waiter object to display progress. Default is NULL.
#'
#' @return Nothing. `send_to_db()` is called for its side effects.
send_to_db <- function(data, conn, waiter = NULL) {

  for (i in 1:length(data)) {

    table_name <- names(data)[i]

    if (!is.null(waiter)) {
      waiter$update(
        spinner_message(
          glue::glue("Step 5/5: Sending data... ({ i } of { length(data) })")
        )
      )
    }

    DBI::dbWriteTable(
      conn = conn,
      table_name,
      data[[table_name]],
      append = TRUE
    )

  }

}

# Create "safe" equivalents for each function
process_data_safely <- purrr::safely(process_data)
prep_tables_safely <- purrr::safely(prep_tables)
delete_from_db_safely <- purrr::safely(delete_from_db)
send_to_db_safely <- purrr::safely(send_to_db)
