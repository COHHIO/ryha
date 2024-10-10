# This script resets the 'dev' db from the 'prod' db

# Establish connection to "prod" database
prod_con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PWD")
)

# Establish connection to "dev" database (when Dev Container is running)
dev_con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = "ryha-dev",
    host = "localhost",
    port = 5432,
    user = "ryha-dev",
    password = "ryha"
)

# Get tables from "prod" database as a list of R data frames
prod_tbl_names <- DBI::dbListTables(conn = prod_con)
prod_data <- vector("list", length = length(prod_tbl_names))
names(prod_data) <- prod_tbl_names

for (t in names(prod_data)) {
  prod_data[[t]] <- DBI::dbReadTable(conn = prod_con, name = t)
}

# Truncate tables in "dev" database
for (t in DBI::dbListTables(conn = dev_con)) {
  table_name <- glue::glue_sql(t, .con = dev_con)
  sql_stmt <- glue::glue_sql("TRUNCATE {table_name}", .con = dev_con)
  DBI::dbExecute(conn = dev_con, statement = sql_stmt)
}

send_to_db(data = prod_data, conn = dev_con)
