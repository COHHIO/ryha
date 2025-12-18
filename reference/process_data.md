# Process HMIS Data

Process HMIS Data

## Usage

``` r
process_data(file)
```

## Arguments

- file:

  String, the full path to the .zip file containing the quarterly HMIS
  data

## Value

A list containing the ingested data for each HMIS table.

## Details

This is the *"master"* function that governs the entire ETL process for
processing the uploaded data and, depending on the user (i.e., whether
or not the user is an approved grantee), writing the data out to the
PostgreSQL database.
