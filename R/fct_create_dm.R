


#' Connect to PostgreSQL Database
#'
#' @description Establish a `{DBI}` database connection to the PostgreSQL
#'   database, using environment variables for the connection information
#'
#' @return A {DBI} database connection
#'
#' @examples
#' \dontrun{
#' con <- connect_to_db()
#' }
connect_to_db <- function() {

  DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PWD")
  )

}


#' Create the data model
#'
#' @description Create a
#'
#' @return List of data frames, based upon the tables in the PostgreSQL database,
#'   with some minor manipulations to reduce the number of data transformations
#'   on-the-fly in the server-side of the app
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dm <- create_dm()
#' }
create_dm <- function() {

  # Establish connection to PostgreSQL database
  con <- connect_to_db()

  # Read "project" table into memory
  project <- DBI::dbReadTable(
    conn = con,
    name = "project"
  )

  # Read "client" table into memory
  client_tbl <- DBI::dbReadTable(
    conn = con,
    name = "client"
  )

  # Prep "client" table
  client <- client_tbl |>
    dplyr::mutate(
      age = lubridate::time_length(
        difftime(lubridate::today(), dob),
        "years"
      ) |> floor()
    ) |>
    dplyr::select(
      personal_id,
      ssn,
      ssn_data_quality,
      age,
      veteran_status,
      organization_id,
      date_updated
    )

  # Prep "gender" table
  gender <- client_tbl |>
    dplyr::select(
      personal_id,
      female:questioning,
      organization_id
    ) |>
    tidyr::pivot_longer(
      cols = female:questioning,
      names_to = "gender",
      values_drop_na = TRUE
    ) |>
    dplyr::filter(value == "Yes") |>
    dplyr::select(-value) |>
    dplyr::right_join(
      client |> dplyr::select(-c(age, veteran_status)),
      by = c("personal_id", "organization_id")
    ) |>
    dplyr::mutate(
      gender = dplyr::if_else(
        is.na(gender),
        "Missing Data",
        stringr::str_replace_all(gender, "_", " ") |> tools::toTitleCase()
      )
    ) |>
    dplyr::arrange(
      organization_id,
      personal_id
    )

  # Prep "ethnicity" table
  ethnicity <- client_tbl |>
    dplyr::select(
      personal_id,
      am_ind_ak_native:white, hispanic_latinaox,
      organization_id,
    ) |>
    tidyr::pivot_longer(
      cols = am_ind_ak_native:hispanic_latinaox,
      names_to = "ethnicity",
      values_drop_na = TRUE
    ) |>
    dplyr::filter(value == "Yes") |>
    dplyr::select(-value) |>
    dplyr::right_join(
      client |> dplyr::select(-c(age, veteran_status)),
      by = c("personal_id", "organization_id")
    ) |>
    dplyr::mutate(
      ethnicity = dplyr::if_else(
        ethnicity == "race_none" | is.na(ethnicity),
        "Missing Data",
        stringr::str_replace_all(ethnicity, "_", " ") |> tools::toTitleCase()
      )
    ) |>
    dplyr::arrange(
      organization_id,
      personal_id
    )

  # Read "current_living_situation" table into memory
  current_living_situation <- DBI::dbReadTable(
    conn = con,
    name = "living"
  )

  disabilities <- DBI::dbReadTable(
    conn = con,
    name = "disabilities"
  )

  employment <- DBI::dbReadTable(
    conn = con,
    name = "employment"
  )

  education <- DBI::dbReadTable(
    conn = con,
    name = "education"
  )

  enrollment <- DBI::dbReadTable(
    conn = con,
    name = "enrollment"
  )

  health <- DBI::dbReadTable(
    conn = con,
    name = "health"
  )

  domestic_violence <- DBI::dbReadTable(
    conn = con,
    name = "domestic_violence"
  )

  income <- DBI::dbReadTable(
    conn = con,
    name = "income"
  )

  benefits <- DBI::dbReadTable(
    conn = con,
    name = "benefits"
  )

  services <- DBI::dbReadTable(
    conn = con,
    name = "services"
  )

  exit <- DBI::dbReadTable(
    conn = con,
    name = "exit"
  )

  # Create {dm} object
  dm <- list(
    project = project,
    client = client,
    gender = gender,
    ethnicity = ethnicity,
    current_living_situation = current_living_situation,
    disabilities = disabilities,
    employment = employment,
    education = education,
    enrollment = enrollment,
    health = health,
    domestic_violence = domestic_violence,
    income = income,
    benefits = benefits,
    services = services,
    exit = exit
  )

  return(dm)

}

#' Read data from table
#'
#' \code{read_data_from_table()} reads specific data from a SQL table based on
#' provided column names.
#'
#' @param connection A DBI database connection object.
#' @param table_name The name of the SQL table from which to read the data.
#' @param column_names A character vector specifying the column names to read
#' from the table.
#'
#' @return A data frame containing the requested data from the specified columns
#' in the table.
#'
#' @examples
#' # Establish connection to PostgreSQL database
#' con <- connect_to_db()
#' read_data_from_table(
#'   connection = con,
#'   table_name = "project",
#'   column_names = c("project_name", "project_id")
#' )
read_data_from_table <- function(connection, table_name, column_names) {
  column_names_string <- paste0(column_names, collapse = ", ")

  DBI::dbGetQuery(
    conn = connection,
    statement = glue::glue(
      "SELECT {column_names_string} FROM {table_name}"
    )
  )
}
