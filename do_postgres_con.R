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

# Create the table, 'iris_test'
# DBI::dbWriteTable(
#   conn = con,
#   name = "iris_test",
#   value = iris
# )

DBI::dbListTables(conn = con)

DBI::dbListFields(conn = con, "iris_test")

DBI::dbReadTable(conn = con, "iris_test")

res <- DBI::dbSendQuery(
  conn = con,
  statement = "SELECT * FROM iris_test WHERE \"Species\" = 'setosa'"
)
dbFetch(res)
dbClearResult(res)

DBI::dbDisconnect(conn = con)
