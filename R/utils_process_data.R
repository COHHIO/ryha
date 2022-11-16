


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

  # List the path to each file
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

  # Create a list of the corresponding `read_*()` functions for each file in
  # `files_list`
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

  # Create an empty list to store the ingested data for each table
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

  # Execute the list of functions against the list of files
  data <- purrr::map2(
    .x = funcs_list,
    .y = files_list,
    .f = rlang::exec
  )

  return(data)

}



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
    "exit"
  )

  return(data[out])

}


delete_from_db <- function(data, conn) {

  # Loop through each table in the database (except 'organization' and 'project')
  # and delete any records that match on the table's `*_id` value and
  # `organization_id` value, when compared to the respective uploaded file data
  for (i in 1:length(data)) {

    # Ensure that there exists a valid 'organization_id' value in the input `data`
    # to use in the DELETE statement's WHERE clause
    if (nrow(data[[i]]) >= 1L) {

      table_name <- glue::glue_sql(
        names(data)[i],
        .con = conn
      )

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



send_to_db <- function(data, conn) {

  for (i in 1:length(data)) {

    table_name <- names(data)[i]

    DBI::dbWriteTable(
      conn = conn,
      table_name,
      data[[table_name]],
      append = TRUE
    )

    # Give the PostgreSQL database a half second to breathe between writes
    # Sys.sleep(0.5)

  }

}
