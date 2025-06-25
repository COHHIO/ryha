# This script drops tables in 'dev' db and recreates them

# Connect to dev db
con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = "ryha-dev",
    host = "localhost",
    port = 5432,
    user = "ryha-dev",
    password = "ryha"
)

# Drop all tables
for (t in DBI::dbListTables(conn = con)) {
    table_name <- glue::glue_sql(t, .con = con)
    sql_stmt <- glue::glue_sql("DROP TABLE {table_name}", .con = con)
    DBI::dbExecute(conn = con, statement = sql_stmt)
}

# Create tables
for (file in list.files(here::here("postgres", "create_db"))) {
    DBI::dbExecute(
        conn = con,
        statement = readr::read_file(
            here::here("postgres", "create_db", file)
        )
    )
}
