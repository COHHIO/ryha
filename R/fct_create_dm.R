


create_dm <- function() {

  # Connect to DB
  con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv("POSTGRES_DBNAME"),
    host = Sys.getenv("POSTGRES_HOST"),
    port = Sys.getenv("POSTGRES_PORT"),
    user = Sys.getenv("POSTGRES_USER"),
    password = Sys.getenv("POSTGRES_PWD")
  )

  # Read "project" table into memory
  project <- DBI::dbReadTable(
    conn = con,
    name = "project"
  )

  # Read "submission" table into memory
  submission <- DBI::dbReadTable(
    conn = con,
    name = "submission"
  ) |>
    dplyr::mutate(quarter = paste0(
      lubridate::year(export_start_date),
      " Q",
      lubridate::quarter(export_start_date)
    ))

  # Read "client" table into memory
  client_tbl <- DBI::dbReadTable(
    conn = con,
    name = "client"
  )

  # Prep "client" table
  client <- client_tbl |>
    dplyr::select(
      submission_id,
      personal_id,
      ssn,
      ssn_data_quality,
      dob,
      dob_data_quality,
      veteran_status
    ) |>
    dplyr::mutate(
      age = lubridate::time_length(
        difftime(lubridate::today(), dob),
        "years"
      ) |> floor()
    ) |>
    dplyr::select(
      submission_id,
      personal_id,
      ssn,
      ssn_data_quality,
      age,
      veteran_status
    )

  # Prep "gender" table
  gender <- client_tbl |>
    dplyr::select(
      submission_id,
      personal_id,
      female:questioning
    ) |>
    tidyr::pivot_longer(
      cols = female:questioning,
      names_to = "gender",
      values_drop_na = TRUE
    ) |>
    dplyr::filter(value == "Yes") |>
    dplyr::select(-value) |>
    dplyr::right_join(
      client |> dplyr::select(-age),
      by = c("personal_id", "submission_id")
    ) |>
    dplyr::mutate(
      gender = dplyr::if_else(is.na(gender), "missing data", gender)
    ) |>
    dplyr::arrange(
      submission_id,
      personal_id
    )

  # Prep "ethnicity" table
  ethnicity <- client_tbl |>
    dplyr::select(
      submission_id,
      personal_id,
      am_ind_ak_native:white, hispanic_latinaox
    ) |>
    tidyr::pivot_longer(
      cols = am_ind_ak_native:hispanic_latinaox,
      names_to = "ethnicity",
      values_drop_na = TRUE
    ) |>
    dplyr::filter(value == "Yes") |>
    dplyr::select(-value) |>
    dplyr::right_join(
      client |> dplyr::select(-age),
      by = c("personal_id", "submission_id")
    ) |>
    dplyr::mutate(
      ethnicity = dplyr::if_else(ethnicity == "race_none", "missing data", ethnicity)
    ) |>
    dplyr::arrange(
      submission_id,
      personal_id
    )

  # Read "current_living_situation" table into memory
  current_living_situation <- DBI::dbReadTable(
    conn = con,
    name = "current_living_situation"
  )

  disabilities <- DBI::dbReadTable(
    conn = con,
    name = "disabilities"
  )

  # Disconnect from the database
  DBI::dbDisconnect(conn = con)

  # Create {dm} object
  dm <- dm::dm(
    project,
    submission,
    client,
    gender,
    ethnicity,
    current_living_situation,
    disabilities
  ) |>
    # setup "project" table
    dm::dm_add_pk(table = project, columns = project_id) |>

    # setup "submission" table
    dm::dm_add_pk(table = submission, columns = submission_id) |>
    dm::dm_add_fk(table = submission, columns = project_id, ref_table = project) |>

    # setup "client" table
    dm::dm_add_pk(table = client, columns = c(submission_id, personal_id)) |>
    dm::dm_add_fk(table = client, columns = submission_id, ref_table = submission) |>

    # setup "gender" table
    dm::dm_add_fk(table = gender, columns = c(submission_id, personal_id), ref_table = client) |>

    # setup "ethnicity" table
    dm::dm_add_fk(table = ethnicity, columns = c(submission_id, personal_id), ref_table = client) |>

    # setup "current_living_situation" table
    dm::dm_add_pk(table = current_living_situation, columns = c(submission_id, current_living_sit_id)) |>
    dm::dm_add_fk(table = current_living_situation, columns = c(submission_id, personal_id), ref_table = client) |>
    # dm::dm_add_fk(table = current_living_situation, columns = c(submission_id, enrollment_id), ref_table = enrollment) |>

    # setup "disabilities" table
    dm::dm_add_pk(table = disabilities, columns = c(submission_id, disabilities_id)) |>
    dm::dm_add_fk(table = disabilities, columns = c(submission_id, personal_id), ref_table = client) # |>
    # dm::dm_add_fk(table = disabilities, columns = c(submission_id, enrollment_id), ref_table = enrollment) |>

  return(dm)

}
