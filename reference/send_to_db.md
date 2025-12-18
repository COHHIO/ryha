# Send data to a database

`send_to_db()` appends each data frame in a list to the corresponding
table in a database.

## Usage

``` r
send_to_db(data, conn, waiter = NULL)
```

## Arguments

- data:

  List of dataframes returned by
  [`prep_tables()`](https://cohhio.github.io/ryha/reference/prep_tables.md).

- conn:

  A database connection object.

- waiter:

  An optional waiter object to display progress. Default is NULL.

## Value

Nothing. `send_to_db()` is called for its side effects.
