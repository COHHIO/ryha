


create_dm <- function() {

  # Establish connection to PostgreSQL database
  con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv("AWS_POSTGRES_DBNAME"),
    host = Sys.getenv("AWS_POSTGRES_HOST"),
    port = Sys.getenv("AWS_POSTGRES_PORT"),
    user = Sys.getenv("AWS_POSTGRES_USER"),
    password = Sys.getenv("AWS_POSTGRES_PWD")
  )

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
      software_name
    )

  # Prep "gender" table
  gender <- client_tbl |>
    dplyr::select(
      personal_id,
      female:questioning,
      software_name
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
      by = c("personal_id", "software_name")
    ) |>
    dplyr::mutate(
      gender = dplyr::if_else(is.na(gender), "missing data", gender)
    ) |>
    dplyr::arrange(
      software_name,
      personal_id
    )

  # Prep "ethnicity" table
  ethnicity <- client_tbl |>
    dplyr::select(
      personal_id,
      am_ind_ak_native:white, hispanic_latinaox,
      software_name,
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
      by = c("personal_id", "software_name")
    ) |>
    dplyr::mutate(
      ethnicity = dplyr::if_else(ethnicity == "race_none", "missing data", ethnicity)
    ) |>
    dplyr::arrange(
      software_name,
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
