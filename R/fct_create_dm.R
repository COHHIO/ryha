#' Connect to PostgreSQL Database
#'
#' @description Establish a `{DBI}` database connection to the PostgreSQL
#' database, using environment variables for the connection information
#'
#' @param env Character. Indicates the database to connect to.
#' Valid options include `"prod"` and `"dev"`:
#' - `"prod"` connects to the database in production
#' - `"dev"` connects to an internal database that is set up using dev containers
#'
#' @return A {DBI} database connection
#'
#' @examples
#' \dontrun{
#' con <- connect_to_db(env = "prod")
#' }
connect_to_db <- function(env) {

  if (!env %in% c("prod", "dev")) {
    rlang::abort("`env` should be one of \"prod\" or \"dev\" to connect to a database")
  }

  if (env == "prod") {
    DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = Sys.getenv("DB_NAME"),
      host = Sys.getenv("DB_HOST"),
      port = Sys.getenv("DB_PORT"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PWD")
    )
  } else if (env == "dev") {
    DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = "ryha-dev",
      host = "localhost",
      port = 5432,
      user = "ryha-dev",
      password = "ryha"
    )
  }

}


#' Create the data model
#'
#' @description `create_dm()` creates a list of data frames from a determined
#' environment.
#'
#' @param env Character. Indicates the environment used to create the data model.
#' Valid options include `"prod"`, `"dev"` and `"file"`:
#' - `"prod"` connects to the database in production
#' - `"dev"` connects to an internal database that is set up using dev containers
#' - `"file"` reads a dm object snapshot
#' @param file Character. Path to a `.rds` file that represents a dm object snapshot.
#' Defaults to the last element in `db_data` directory.
#'
#' @details
#' When `env` is `"prod"` or `"dev"`, `create_dm()` connects to the database and,
#' for each table, reads the columns used in the app. `gender` and `ethnicity`
#' data frames are derived from `client` table.
#'
#' To create the `.rds` object required when `env` is `"file"`, a person with access
#' to the database in production should save and share the `dm` object that is
#' generated in `app_server.R`.
#'
#' @return List of data frames, based upon the tables in the PostgreSQL database,
#' with some minor manipulations to reduce the number of data transformations
#' on-the-fly in the server-side of the app
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dm <- create_dm(env = "prod")
#' }
create_dm <- function(env,
                      file = list.files("db_data", full.names = TRUE) |> tail(n = 1)) {

  if (!env %in% c("prod", "dev", "file")) {
    rlang::abort("`env` should be one of \"prod\", \"dev\", \"file\"")
  }

  if (env == "file") {

    if (length(file) == 0) stop("Please provide a valid `file`")
    readRDS(file)

  } else {

    # Establish connection to PostgreSQL database
    con <- connect_to_db(env)

    # Read "project" data into memory
    project <- read_data_from_table(
      connection = con,
      table_name = "project",
      column_names = c(
        "project_name",
        "project_id"
      )
    )

    # Read "project coc" data into memory
    project_coc <- read_data_from_table(
      connection = con,
      table_name = "project_coc",
      column_names = c(
        "project_id",
        "coc_code",
        "geocode"
      )
    ) |>
      # Add county column
      dplyr::left_join(CountyCodes, by = "geocode") |>
      # Assign counties without a match to "Unknown"
      tidyr::replace_na(list(county = "Unknown"))

    # Read "funder" data into memory
    funder <- read_data_from_table(
      connection = con,
      table_name = "funder",
      column_names = c(
        "project_id",
        "funder"
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
      )

    # Avoid data wrangling errors when there is no data available
    if (nrow(gender) > 0) {
      gender <- gender |>
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
    }

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
      )

    # Avoid data wrangling errors when there is no data available
    if (nrow(ethnicity) > 0) {
      ethnicity <- ethnicity |>
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
    }

    disabilities <- read_data_from_table(
      connection = con,
      table_name = "disabilities",
      column_names = c(
        "enrollment_id",
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
        "enrollment_id",
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
        "enrollment_id",
        "personal_id",
        "last_grade_completed",
        "school_status",
        "data_collection_stage",
        "date_updated",
        "organization_id"
      )
    ) |>
    dplyr::mutate(
      # Bucket Last Grade Completed categories
      last_grade_completed_grouped = factor(
        last_grade_completed,
        levels = c(
          "Less than Grade 5",
          "Grades 5-6",
          "Grades 7-8",
          "Grades 9-11",
          "Grade 12 / High school diploma",
          "GED",
          "Some College",
          "Associate's Degree",
          "Bachelor's Degree",
          "Graduate Degree",
          "Vocational Degree",
          "School program does not have grade levels",
          "Client doesn't know",
          "Client refused",
          "Data not collected"
        ),
        labels = c(
          "Less than Grade 5",
          "Grades 5-8",
          "Grades 5-8",
          "Grades 9-11",
          "High school diploma/GED",
          "High school diploma/GED",
          "Some College",
          "College Degree/Vocational",
          "College Degree/Vocational",
          "College Degree/Vocational",
          "College Degree/Vocational",
          "Unknown",
          "Unknown",
          "Unknown",
          "Unknown"
        ),
        ordered = TRUE
      ),
      school_status = factor(
        school_status,
        levels = c(
          "Obtained GED",
          "Graduated from high school",
          "Attending school regularly",
          "Attending school irregularly",
          "Suspended",
          "Expelled",
          "Dropped out",
          "Client doesn't know",
          "Client refused",
          "Data not collected"
        ),
        ordered = TRUE
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
        "organization_id",
        "date_updated"
      )
    )

    health <- read_data_from_table(
      connection = con,
      table_name = "health",
      column_names = c(
        "enrollment_id",
        "personal_id",
        "general_health_status",
        "dental_health_status",
        "mental_health_status",
        "pregnancy_status",
        "data_collection_stage",
        "date_updated",
        "organization_id"
      )
    ) |> 
    dplyr::mutate(
      dplyr::across(
        .cols = c(general_health_status, dental_health_status, mental_health_status),
        .fns = function(col) {
          factor(
            col,
            levels = c(
              "Excellent",
              "Very good",
              "Good",
              "Fair",
              "Poor",
              "Client doesn't know",
              "Client prefers not to answer",
              "Data not collected"
            ),
            ordered = TRUE
          )
        }
      )
    )

    domestic_violence <- read_data_from_table(
      connection = con,
      table_name = "domestic_violence",
      column_names = c(
        "enrollment_id",
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
        "enrollment_id",
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
        "enrollment_id",
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
        "enrollment_id",
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
        "organization_id",
        "date_updated"
      )
    )

    # Create {dm} object
    dm <- list(
      project = project,
      project_coc = project_coc,
      funder = funder,
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

}

#' Read specific columns from database table
#'
#' \code{read_data_from_table()} provides an expedited way to query a select,
#' user-specified set of columns from a SQL database table.
#'
#' @param connection A DBI database connection object
#' @param table_name The name of the SQL table from which to read the data
#' @param column_names A character vector specifying the column names to read
#' from the table
#'
#' @return A data frame containing the requested data from the specified columns
#' in the table
#'
#' @examples
#' \dontrun{
#' # Establish connection to PostgreSQL database
#' con <- connect_to_db(env = "prod")
#'
#' # Query only the "project_name" and "project_id" columns from the 'project'
#' # database table
#' read_data_from_table(
#'   connection = con,
#'   table_name = "project",
#'   column_names = c("project_name", "project_id")
#' )
#' }
read_data_from_table <- function(connection, table_name, column_names) {

  DBI::dbGetQuery(
    conn = connection,
    statement = glue::glue_sql(
      "SELECT {`column_names`*} FROM {`table_name`}",
      .con = connection
    )
  )

}
