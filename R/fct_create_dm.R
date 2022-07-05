# In this script we create the baseline dm object.
# As we have to apply some wrangling to the tables, the approach I took was
# the following:
# - Connect to the DB
# - Get each table into memory applying the wrangle I found necessary
#   (create new columns)
# - Disconnect from the DB
# - Use the tables in memory to create the dm object. So far, tables are not
#   that big. I'm not aware of the number of rows of the rest.
# This process takes some time to run (between 10 and 15 seconds), but once
#  the app launches everything works properly.
# We might end up refactoring this code. I wanted to make things work as soon
#  as possible.

create_dm <- function(){
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

  # Create auxiliary ethnicity table
  #  This table contains for each personal_id a comma separated string of ethnicities
  #  If a personal_id had missing data, it won't be in this dataset.
  #  This table will be left joined to the clients table. personal_id without a match
  #  will be assigned the value "Missing Data".
  aux_table_client_ethnicity <- DBI::dbReadTable(conn = con, name = "client") |>
    tidyr::pivot_longer(
      cols = c(am_ind_ak_native,
               asian,
               black_af_american,
               native_hi_pacific,
               white,
               race_none,
               hispanic_latinaox),
      names_to = "ethnicity_name",
      values_to = "has_ethnicity"
    ) |>
    dplyr::mutate(
      ethnicity_name = dplyr::case_when(
        ethnicity_name == "am_ind_ak_native" ~ "American Indian and Alaska Native",
        ethnicity_name == "asian" ~ "Asian",
        ethnicity_name == "black_af_american" ~ "Black and African American",
        ethnicity_name == "native_hi_pacific" ~ "Native Hawaiians and other Pacific Islanders",
        ethnicity_name == "white" ~ "White",
        ethnicity_name == "race_none" ~ "Missing Data",
        ethnicity_name == "hispanic_latinaox" ~ "Hispanic and Latino American",
        TRUE ~ "Missing Data"
      )
    ) |>
    dplyr::filter(has_ethnicity == "Yes") |>
    dplyr::group_by(submission_id, personal_id) |>
    dplyr::summarise(ethnicity = paste0(ethnicity_name, collapse = ",")) |>
    dplyr::ungroup()

  table_client <- DBI::dbReadTable(conn = con, name = "client") |>
    dplyr::left_join(
      y = aux_table_client_ethnicity,
      by = c("submission_id", "personal_id")
    ) |>
    dplyr::mutate(

      # Identify Missing Data in ethnicity column
      ethnicity = dplyr::case_when(
        is.na(ethnicity) ~ "Missing Data",
        TRUE ~ ethnicity
      ),

      # Create gender column
      gender = dplyr::case_when(
        transgender == "Yes" ~ "Transgender",
        questioning == "Yes" ~ "Questioning",
        no_single_gender == "Yes" ~ "No Single Gender",
        female == "Yes" ~ "Female",
        male == "Yes" ~ "Male",
        TRUE ~ "Missing Data"
      ),

      # Create veteran status column
      veteran_status = dplyr::case_when(
        veteran_status == "Yes" ~ "Yes",
        veteran_status == "No" ~ "No",
        TRUE ~ "Missing Data"
      ),

      # Create age column
      age = lubridate::interval(dob, lubridate::today()) / lubridate::years(1)
    )

  table_submission <- DBI::dbReadTable(conn = con, name = "submission") |>
    dplyr::mutate(
      # Create submission quarter column
      quarter = paste0(
        lubridate::year(export_start_date),
        " Q",
        lubridate::quarter(export_start_date)
      )
    )

  table_project <- DBI::dbReadTable(conn = con, name = "project")
  table_current_living_situation <- DBI::dbReadTable(conn = con, name = "current_living_situation")

  DBI::dbDisconnect(conn = con)

  # Create dm object, defining keys
  my_dm <- dm::dm(table_client,
                  table_submission,
                  table_project,
                  table_current_living_situation) |>
    dm::dm_add_pk(table = table_project, columns = submission_id) |>
    dm::dm_add_pk(table = table_client, columns = personal_id) |>
    dm::dm_add_fk(table = table_client, columns = submission_id, ref_table = table_project) |>
    dm::dm_add_fk(table = table_submission, columns = submission_id, ref_table = table_project) |>
    dm::dm_add_fk(table = table_current_living_situation, columns = personal_id, ref_table = table_client)

  return(my_dm)

}
