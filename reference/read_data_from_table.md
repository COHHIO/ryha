# Read specific columns from database table

`read_data_from_table()` provides an expedited way to query a select,
user-specified set of columns from a SQL database table.

## Usage

``` r
read_data_from_table(connection, table_name, column_names)
```

## Arguments

- connection:

  A DBI database connection object

- table_name:

  The name of the SQL table from which to read the data

- column_names:

  A character vector specifying the column names to read from the table

## Value

A data frame containing the requested data from the specified columns in
the table

## Examples

``` r
if (FALSE) { # \dontrun{
# Establish connection to PostgreSQL database
con <- connect_to_db(env = "prod")

# Query only the "project_name" and "project_id" columns from the 'project'
# database table
read_data_from_table(
    connection = con,
    table_name = "project",
    column_names = c("project_name", "project_id")
)
} # }
```
