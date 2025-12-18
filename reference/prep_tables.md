# Prepare data for database tasks

`prep_tables()` processes the list returned by
[`process_data()`](https://cohhio.github.io/ryha/reference/process_data.md)
and returns a new list which is used by
[`delete_from_db()`](https://cohhio.github.io/ryha/reference/delete_from_db.md)
and
[`send_to_db()`](https://cohhio.github.io/ryha/reference/send_to_db.md).

## Usage

``` r
prep_tables(data, conn)
```

## Arguments

- data:

  List of dataframes returned by
  [`process_data()`](https://cohhio.github.io/ryha/reference/process_data.md).

- conn:

  A database connection object.

## Value

A list of processed data frames.

## Details

`prep_tables()` handles the addition of new organization and project
entries to the database, adds `project_id` and `organization_id` columns
to `enrollment` file data, adds `organization_id` to the remaining files
(e.g. client, disabilities, education, ...) and returns them as a list
of dataframes.

`organization` and `project` dataframes are excluded from the returned
list.
