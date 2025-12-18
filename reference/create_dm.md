# Create the data model

`create_dm()` creates a list of data frames from a determined
environment.

## Usage

``` r
create_dm(env, file = tail(list.files("db_data", full.names = TRUE), n = 1))
```

## Arguments

- env:

  Character. Indicates the environment used to create the data model.
  Valid options include `"prod"`, `"dev"` and `"file"`:

  - `"prod"` connects to the database in production

  - `"dev"` connects to an internal database that is set up using dev
    containers

  - `"file"` reads a dm object snapshot

- file:

  Character. Path to a `.rds` file that represents a dm object snapshot.
  Defaults to the last element in `db_data` directory.

## Value

List of data frames, based upon the tables in the PostgreSQL database,
with some minor manipulations to reduce the number of data
transformations on-the-fly in the server-side of the app

## Details

When `env` is `"prod"` or `"dev"`, `create_dm()` connects to the
database and, for each table, reads the columns used in the app.
`ethnicity` data frames is derived from `client` table.

To create the `.rds` object required when `env` is `"file"`, a person
with access to the database in production should save and share the `dm`
object that is generated in `app_server.R`.

## Examples

``` r
if (FALSE) { # \dontrun{
dm <- create_dm(env = "prod")
} # }
```
