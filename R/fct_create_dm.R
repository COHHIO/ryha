# Connect to DB
con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("POSTGRES_DBNAME"),
  host = Sys.getenv("POSTGRES_HOST"),
  port = Sys.getenv("POSTGRES_PORT"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PWD")
)

# Data wrangling
#  Read the tables into memory and process them. This might not be ideal, but
#  table size is small for now.
table_client <- DBI::dbReadTable(conn = con, name = "client") |>
  dplyr::mutate(
    ethnicity = dplyr::case_when(
      am_ind_ak_native == "Yes" ~ "American Indian and Alaska Native",
      asian == "Yes" ~ "Asian",
      black_af_american == "Yes" ~ "Black and African American",
      native_hi_pacific == "Yes" ~ "Native Hawaiians and other Pacific Islanders",
      white == "Yes" ~ "White",
      !is.na(race_none) ~ "Missing Data",
      hispanic_latinaox == "Yes" ~ "Hispanic and Latino American",
      TRUE ~ "Missing Data"
    ),

    gender = dplyr::case_when(
      transgender == "Yes" ~ "Transgender",
      questioning == "Yes" ~ "Questioning",
      no_single_gender == "Yes" ~ "No Single Gender",
      female == "Yes" ~ "Female",
      male == "Yes" ~ "Male",
      TRUE ~ "Missing Data"
    ),

    veteran_status = dplyr::case_when(
      veteran_status == "Yes" ~ "Yes",
      veteran_status == "No" ~ "No",
      TRUE ~ "Missing Data"
    )
  )

table_submission <- DBI::dbReadTable(conn = con, name = "submission")
table_project <- DBI::dbReadTable(conn = con, name = "project")
table_current_living_situation <- DBI::dbReadTable(conn = con, name = "current_living_situation")

DBI::dbDisconnect(conn = con)

my_dm <- dm::dm(table_client,
                table_submission,
                table_project,
                table_current_living_situation) |>
  dm::dm_add_pk(table = table_submission, columns = submission_id) |>
  dm::dm_add_pk(table = table_client, columns = personal_id) |>
  dm::dm_add_fk(table = table_client, columns = submission_id, ref_table = table_submission) |>
  dm::dm_add_fk(table = table_current_living_situation, columns = personal_id, ref_table = table_client)

