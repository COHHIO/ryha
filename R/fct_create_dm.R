


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

  # Read "project" data into memory
  project <- read_data_from_table(
    connection = con,
    table_name = "project",
    column_names = c(
      "project_name",
      "project_id"
    )
  )

  # Read "client" table into memory
client_tbl <- read_data_from_table(
    connection = con,
    table_name = "client",
    column_names = c(
      "personal_id",
      "ssn",
      "ssn_data_quality",
      "dob",
      "am_ind_ak_native",
      "asian",
      "black_af_american",
      "hispanic_latinaeo",
      "mid_east_n_african",
      "native_hi_pacific",
      "white",
      "race_none",
      "woman",
      "man",
      "non_binary",
      "culturally_specific",
      "transgender",
      "questioning",
      "different_identity",
      "veteran_status",
      "organization_id",
      "date_updated"
    )
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
      woman,
      man,
      non_binary,
      culturally_specific,
      transgender,
      questioning,
      different_identity,
      organization_id
    ) |>
    tidyr::pivot_longer(
      cols = c(
        woman,
        man,
        non_binary,
        culturally_specific,
        transgender,
        questioning,
        different_identity
      ),
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
      am_ind_ak_native,
      asian,
      black_af_american,
      hispanic_latinaeo,
      mid_east_n_african,
      native_hi_pacific,
      white,
      race_none,
      organization_id,
    ) |>
    tidyr::pivot_longer(
      cols = c(
        am_ind_ak_native,
        asian,
        black_af_american,
        hispanic_latinaeo,
        mid_east_n_african,
        native_hi_pacific,
        white,
        race_none
      ),
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

  disabilities <- read_data_from_table(
    connection = con,
    table_name = "disabilities",
    column_names = c(
      "personal_id",
      "disability_type",
      "disability_response",
      "data_collection_stage",
      "date_updated",
      "organization_id"
    )
  )

  employment <- read_data_from_table(
    connection = con,
    table_name = "employment",
    column_names = c(
      "personal_id",
      "employed",
      "employment_type",
      "not_employed_reason",
      "data_collection_stage",
      "date_updated",
      "organization_id"
    )
  )

  education <- read_data_from_table(
    connection = con,
    table_name = "education",
    column_names = c(
      "personal_id",
      "last_grade_completed",
      "school_status",
      "data_collection_stage",
      "date_updated",
      "organization_id"
    )
  )

  enrollment <- read_data_from_table(
    connection = con,
    table_name = "enrollment",
    column_names = c(
      "enrollment_id",
      "personal_id",
      "entry_date",
      "household_id",
      "relationship_to_ho_h",
      "living_situation",
      "referral_source",
      "sexual_orientation",
      "former_ward_child_welfare",
      "former_ward_juvenile_justice",
      "project_id",
      "organization_id"
    )
  )

  health <- read_data_from_table(
    connection = con,
    table_name = "health",
    column_names = c(
      "personal_id",
      "general_health_status",
      "dental_health_status",
      "mental_health_status",
      "pregnancy_status",
      "data_collection_stage",
      "date_updated",
      "organization_id"
    )
  )

  domestic_violence <- read_data_from_table(
    connection = con,
    table_name = "domestic_violence",
    column_names = c(
      "personal_id",
      "domestic_violence_survivor",
      "when_occurred",
      "currently_fleeing",
      "data_collection_stage",
      "date_updated",
      "organization_id"
    )
  )

  income <- read_data_from_table(
    connection = con,
    table_name = "income",
    column_names = c(
      "personal_id",
      "income_from_any_source",
      "total_monthly_income",
      "earned",
      "unemployment",
      "ssi",
      "ssdi",
      "va_disability_service",
      "va_disability_non_service",
      "private_disability",
      "workers_comp",
      "tanf",
      "ga",
      "soc_sec_retirement",
      "pension",
      "child_support",
      "alimony",
      "other_income_source",
      "data_collection_stage",
      "date_updated",
      "organization_id"
    )
  )

  benefits <- read_data_from_table(
    connection = con,
    table_name = "benefits",
    column_names = c(
      "personal_id",
      "benefits_from_any_source",
      "snap",
      "wic",
      "tanf_child_care",
      "tanf_transportation",
      "other_tanf",
      "other_benefits_source",
      "insurance_from_any_source",
      "medicaid",
      "medicare",
      "schip",
      "vha_services",
      "employer_provided",
      "cobra",
      "private_pay",
      "state_health_ins",
      "indian_health_services",
      "other_insurance",
      "data_collection_stage",
      "date_updated",
      "organization_id"
    )
  )

  services <- read_data_from_table(
    connection = con,
    table_name = "services",
    column_names = c(
      "personal_id",
      "date_provided",
      "type_provided",
      "organization_id"
    )
  )

  exit <- read_data_from_table(
    connection = con,
    table_name = "exit",
    column_names = c(
      "enrollment_id",
      "personal_id",
      "exit_date",
      "destination",
      "project_completion_status",
      "exchange_for_sex",
      "count_of_exchange_for_sex",
      "asked_or_forced_to_exchange_for_sex",
      "work_place_violence_threats",
      "workplace_promise_difference",
      "coerced_to_continue_work",
      "counseling_received",
      "destination_safe_client",
      "destination_safe_worker",
      "organization_id"
    )
  )

  # Create {dm} object
  dm <- list(
    project = project,
    client = client,
    gender = gender,
    ethnicity = ethnicity,
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
