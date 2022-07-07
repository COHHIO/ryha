


con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("POSTGRES_DBNAME"),
  host = Sys.getenv("POSTGRES_HOST"),
  port = Sys.getenv("POSTGRES_PORT"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PWD")
)

project <- DBI::dbReadTable(conn = con, name = "project") |>
  dplyr::select(-submission_id) |>
  dplyr::distinct(.keep_all = TRUE)


submission <- DBI::dbReadTable(conn = con, name = "submission")


client_tbl <- DBI::dbReadTable(conn = con, name = "client")


client <- client_tbl |>
  dplyr::select(
    submission_id,
    personal_id,
    dob,
    dob_data_quality
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
    age
  )

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


current_living_situation <- DBI::dbReadTable(conn = con, name = "current_living_situation")


dm <- dm::dm(
  project,
  submission,
  client,
  gender,
  ethnicity,
  current_living_situation
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
  dm::dm_add_fk(table = current_living_situation, columns = c(submission_id, personal_id), ref_table = client) # |>
  # dm::dm_add_fk(table = current_living_situation, columns = c(submission_id, enrollment_id), ref_table = enrollment) |>


# TEST IT OUT!
dm |>
  dm::dm_filter(client, age > 18) |>
  dm::dm_filter(gender, gender == "male") |>
  dm::dm_filter(project, project_name == "YWCA - ODH") |>
  dm::dm_apply_filters() |>
  dm::pull_tbl(ethnicity)

