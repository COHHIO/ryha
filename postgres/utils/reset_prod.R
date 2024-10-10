
# devtools::load_all()
con <- connect_to_db()

# Re-populate tables ----
data <- process_data(file = "app_testing/app_testing/hudx-111_1667240256.zip")
data <- prep_tables(data = data, conn = con)
delete_from_db(data = data, conn = con)
send_to_db(data = data, conn = con)

# Truncate all tables ----
for (t in DBI::dbListTables(conn = con)) {
  table_name <- glue::glue_sql(t, .con = con)
  sql_stmt <- glue::glue_sql("TRUNCATE {table_name}", .con = con)
  DBI::dbExecute(conn = con, statement = sql_stmt)
}

# Drop all tables ----
for (t in DBI::dbListTables(conn = con)) {
  table_name <- glue::glue_sql(t, .con = con)
  sql_stmt <- glue::glue_sql("DROP TABLE {table_name}", .con = con)
  DBI::dbExecute(conn = con, statement = sql_stmt)
}

# Delete a specific organization ----
for (t in DBI::dbListTables(conn = con)) {

  table_name <- glue::glue_sql(t, .con = con)

  # Change "3" in the statement below to the appropriate `organization_id` of
  # the organization you want to remove
  sql_stmt <- glue::glue_sql(
    "
      DELETE FROM {table_name}
      WHERE organization_id = 3
    ",
    .con = con
  )

  DBI::dbExecute(conn = con, statement = sql_stmt)
}

# Create all tables ----
for (file in list.files(here::here("postgres", "create_db"))) {
  DBI::dbExecute(
    conn = con,
    statement = readr::read_file(
      here::here("postgres", "create_db", file)
    )
  )

  DBI::dbExecute(
    conn = con,
    statement = glue::glue(
      "ALTER TABLE IF EXISTS public.{ tools::file_path_sans_ext(file) }
         OWNER to cohhiodbadmin;"
    )
  )

}
