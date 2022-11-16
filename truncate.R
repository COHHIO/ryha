


con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("AWS_POSTGRES_DBNAME"),
  host = Sys.getenv("AWS_POSTGRES_HOST"),
  port = Sys.getenv("AWS_POSTGRES_PORT"),
  user = Sys.getenv("AWS_POSTGRES_USER"),
  password = Sys.getenv("AWS_POSTGRES_PWD")
)

# Truncate all tables
for (t in DBI::dbListTables(conn = con)) {

  table_name <- glue::glue_sql(
    t,
    .con = con
  )

  sql_stmt <- glue::glue_sql(
    "
      TRUNCATE {table_name}
    ",
    .con = con
  )

  DBI::dbExecute(
    conn = con,
    statement = sql_stmt
  )

}

# Re-populate tables
process_data(file = "data/app_testing/one_org.zip") |>
  prep_tables(conn = con) |>
  send_to_db(conn = con)

# Or, drop all tables
# Truncate all tables
for (t in DBI::dbListTables(conn = con)) {

  table_name <- glue::glue_sql(
    t,
    .con = con
  )

  sql_stmt <- glue::glue_sql(
    "
      DROP TABLE {table_name}
    ",
    .con = con
  )

  DBI::dbExecute(
    conn = con,
    statement = sql_stmt
  )

}


