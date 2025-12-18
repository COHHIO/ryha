# Ingest "Funder.csv" file and perform ETL prep for "FUNDER" database table

Ingest "Funder.csv" file and perform ETL prep for "FUNDER" database
table

## Usage

``` r
read_funder(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"FUNDER" database tables

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/Funder.csv"

read_funder(
    file = path
)
} # }
```
