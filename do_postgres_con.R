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

DBI::dbListTables(conn = con)

DBI::dbListFields(conn = con, "client")

DBI::dbReadTable(conn = con, "client") |> tibble::as_tibble()

DBI::dbReadTable(conn = con, "project") |> tibble::as_tibble()

DBI::dbReadTable(conn = con, "submission") |> tibble::as_tibble()

res <- DBI::dbSendQuery(
  conn = con,
  statement = "SELECT COUNT(*) FROM client"
)
DBI::dbFetch(res)
DBI::dbClearResult(res)

DBI::dbDisconnect(conn = con)
