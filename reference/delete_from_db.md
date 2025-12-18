# Delete records from a database

`delete_from_db()` deletes records from a database based on uploaded
data.

## Usage

``` r
delete_from_db(data, conn)
```

## Arguments

- data:

  List of dataframes returned by
  [`prep_tables()`](https://cohhio.github.io/ryha/reference/prep_tables.md).

- conn:

  A database connection object.

## Value

Nothing. `delete_from_db()` is called for its side effects.

## Details

`delete_from_db()` iterates through each table in the database (except
for 'organization' and 'project') and deletes records that match on the
table's `*_id` value and `organization_id` value, when compared to the
respective uploaded file data.
