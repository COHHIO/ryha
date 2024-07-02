# Connect to dev database
con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "ryha-dev",
  host = "localhost",
  port = 5432,
  user = "ryha-dev",
  password = "ryha"
)

# Create tables
for (file in list.files(here::here("postgres", "create_db"))) {
  DBI::dbExecute(
    conn = con,
    statement = readr::read_file(
      here::here("postgres", "create_db", file)
    )
  )
}

# Read data snapshot
## dm.rds is a snapshot of the database in production.
## It needs to be created by someone with access to production database.
## The process to generate this object is to read each table in the database
## into a list of dataframes where each element is named after the table name
## the data was read from
data <- readRDS("postgres/populate_dev_database/data/dm.rds")

# Send to database
## Remember to run devtools::load_all(".") for the function to be available
send_to_db(data = data, conn = con)
