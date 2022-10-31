


create_dm <- function(conn) {

  # Read "project" table into memory
  project <- DBI::dbReadTable(
    conn = conn,
    name = "project"
  )

  # Read "submission" table into memory
  submission <- DBI::dbReadTable(
    conn = conn,
    name = "submission"
  ) |>
    dplyr::mutate(quarter = paste0(
      lubridate::year(export_start_date),
      " Q",
      lubridate::quarter(export_start_date)
    ))

  # Read "client" table into memory
  client_tbl <- DBI::dbReadTable(
    conn = conn,
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
      submission_id,
      software_name,
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
      software_name,
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
      client |> dplyr::select(-c(age, veteran_status)),
      by = c("personal_id", "submission_id", "software_name")
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
      software_name,
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
      client |> dplyr::select(-c(age, veteran_status)),
      by = c("personal_id", "submission_id", "software_name")
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
    conn = conn,
    name = "living"
  )

  disabilities <- DBI::dbReadTable(
    conn = conn,
    name = "disabilities"
  )

  employment <- DBI::dbReadTable(
    conn = conn,
    name = "employment"
  )

  education <- DBI::dbReadTable(
    conn = conn,
    name = "education"
  )

  enrollment <- DBI::dbReadTable(
    conn = conn,
    name = "enrollment"
  ) |>
    # TODO // This needs to be fixed on the "process_data" side
    dplyr::mutate(submission_id = project_id)

  health <- DBI::dbReadTable(
    conn = conn,
    name = "health"
  )

  domestic_violence <- DBI::dbReadTable(
    conn = conn,
    name = "domestic_violence"
  )

  income <- DBI::dbReadTable(
    conn = conn,
    name = "income"
  )

  benefits <- DBI::dbReadTable(
    conn = conn,
    name = "benefits"
  )

  services <- DBI::dbReadTable(
    conn = conn,
    name = "services"
  )

  exit <- DBI::dbReadTable(
    conn = conn,
    name = "exit"
  )

  # Disconnect from the database
  DBI::dbDisconnect(conn = conn)

  # Create {dm} object
  dm <- dm::dm(
    project,
    submission,
    client,
    gender,
    ethnicity,
    current_living_situation,
    disabilities,
    employment,
    education,
    enrollment,
    health,
    domestic_violence,
    income,
    benefits,
    services,
    exit
  ) |>
    # setup "project" table
    dm::dm_add_pk(table = project, columns = project_id) |>

    # setup "submission" table
    dm::dm_add_pk(table = submission, columns = submission_id) |>

    # setup "client" table
    dm::dm_add_pk(table = client, columns = c(submission_id, personal_id, software_name)) |>
    dm::dm_add_fk(table = client, columns = submission_id, ref_table = submission) |>

    # setup "gender" table
    dm::dm_add_fk(table = gender, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    # dm::dm_add_fk(table = gender, columns = submission_id, ref_table = submission) |>

    # setup "ethnicity" table
    dm::dm_add_fk(table = ethnicity, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    # dm::dm_add_fk(table = ethnicity, columns = submission_id, ref_table = submission) |>

    # setup "enrollment" table
    dm::dm_add_pk(table = enrollment, columns = c(submission_id, enrollment_id, software_name)) |>
    dm::dm_add_fk(table = enrollment, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    dm::dm_add_fk(table = enrollment, columns = project_id, ref_table = project) |>
    # dm::dm_add_fk(table = enrollment, columns = submission_id, ref_table = submission) |>

    # setup "current_living_situation" table
    dm::dm_add_pk(table = current_living_situation, columns = c(submission_id, current_living_sit_id, software_name)) |>
    dm::dm_add_fk(table = current_living_situation, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    dm::dm_add_fk(table = current_living_situation, columns = c(submission_id, enrollment_id, software_name), ref_table = enrollment) |>
    # dm::dm_add_fk(table = current_living_situation, columns = submission_id, ref_table = submission) |>

    # setup "disabilities" table
    dm::dm_add_pk(table = disabilities, columns = c(submission_id, disabilities_id, software_name)) |>
    dm::dm_add_fk(table = disabilities, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    dm::dm_add_fk(table = disabilities, columns = c(submission_id, enrollment_id, software_name), ref_table = enrollment) |>
    # dm::dm_add_fk(table = disabilities, columns = submission_id, ref_table = submission) |>

    # setup "employment" table
    dm::dm_add_pk(table = employment, columns = c(submission_id, employment_education_id, software_name)) |>
    dm::dm_add_fk(table = employment, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    dm::dm_add_fk(table = employment, columns = c(submission_id, enrollment_id, software_name), ref_table = enrollment) |>
    # dm::dm_add_fk(table = employment, columns = submission_id, ref_table = submission) |>

    # setup "education" table
    dm::dm_add_pk(table = education, columns = c(submission_id, employment_education_id, software_name)) |>
    dm::dm_add_fk(table = education, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    dm::dm_add_fk(table = education, columns = c(submission_id, enrollment_id, software_name), ref_table = enrollment) |>
    # dm::dm_add_fk(table = education, columns = submission_id, ref_table = submission) |>

    # setup "health" table
    dm::dm_add_pk(table = health, columns = c(submission_id, health_and_dvid, software_name)) |>
    dm::dm_add_fk(table = health, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    dm::dm_add_fk(table = health, columns = c(submission_id, enrollment_id, software_name), ref_table = enrollment) |>
    # dm::dm_add_fk(table = health, columns = submission_id, ref_table = submission) |>

    # setup "domestic_violence" table
    dm::dm_add_pk(table = domestic_violence, columns = c(submission_id, health_and_dvid, software_name)) |>
    dm::dm_add_fk(table = domestic_violence, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    dm::dm_add_fk(table = domestic_violence, columns = c(submission_id, enrollment_id, software_name), ref_table = enrollment) |>
    # dm::dm_add_fk(table = domestic_violence, columns = submission_id, ref_table = submission) |>

    # setup "income" table
    dm::dm_add_pk(table = income, columns = c(submission_id, income_benefits_id, software_name)) |>
    dm::dm_add_fk(table = income, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    dm::dm_add_fk(table = income, columns = c(submission_id, enrollment_id, software_name), ref_table = enrollment) |>
    # dm::dm_add_fk(table = income, columns = submission_id, ref_table = submission) |>

    # setup "benefits" table
    dm::dm_add_pk(table = benefits, columns = c(submission_id, income_benefits_id, software_name)) |>
    dm::dm_add_fk(table = benefits, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    dm::dm_add_fk(table = benefits, columns = c(submission_id, enrollment_id, software_name), ref_table = enrollment) |>
    # dm::dm_add_fk(table = benefits, columns = submission_id, ref_table = submission) |>

    # setup "services" table
    dm::dm_add_pk(table = services, columns = c(submission_id, services_id, software_name)) |>
    dm::dm_add_fk(table = services, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    dm::dm_add_fk(table = services, columns = c(submission_id, enrollment_id, software_name), ref_table = enrollment) |>
    # dm::dm_add_fk(table = services, columns = submission_id, ref_table = submission) |>

    # setup "exit" table
    dm::dm_add_pk(table = exit, columns = c(submission_id, exit_id, software_name)) |>
    dm::dm_add_fk(table = exit, columns = c(submission_id, personal_id, software_name), ref_table = client) |>
    dm::dm_add_fk(table = exit, columns = c(submission_id, enrollment_id, software_name), ref_table = enrollment)
    # dm::dm_add_fk(table = exit, columns = submission_id, ref_table = submission) |>

  return(dm)

}
