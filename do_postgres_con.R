library(DBI)
library(RPostgres)

# Connect to DigitalOcean managed PostgreSQL database
con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("POSTGRES_DBNAME"),
  host = Sys.getenv("POSTGRES_HOST"),
  port = Sys.getenv("POSTGRES_PORT"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PWD")
)

# List all tables in the database
DBI::dbListTables(conn = con)

# List the column names for a particular table
DBI::dbListFields(conn = con, "client")

# Read in all the data from a particular table
DBI::dbReadTable(conn = con, "client") |>
  tibble::as_tibble()

DBI::dbReadTable(conn = con, "project") |>
  tibble::as_tibble()

DBI::dbReadTable(conn = con, "submission") |>
  tibble::as_tibble()

# Send a specific SQL query to the database
res <- DBI::dbSendQuery(
  conn = con,
  statement = "SELECT COUNT(*) FROM client"
)
DBI::dbFetch(res)
DBI::dbClearResult(res)

# Disconnect from the database (for security purposes)
DBI::dbDisconnect(conn = con)
