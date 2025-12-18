# Connect to PostgreSQL Database

Establish a `{DBI}` database connection to the PostgreSQL database,
using environment variables for the connection information

## Usage

``` r
connect_to_db(env)
```

## Arguments

- env:

  Character. Indicates the database to connect to. Valid options include
  `"prod"` and `"dev"`:

  - `"prod"` connects to the database in production

  - `"dev"` connects to an internal database that is set up using dev
    containers

## Value

A DBI database connection

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_to_db(env = "prod")
} # }
```
