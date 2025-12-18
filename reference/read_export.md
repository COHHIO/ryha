# Ingest "Export.csv" file and perform ETL prep for "PROJECT" and "SUBMISSION" database tables

Ingest "Export.csv" file and perform ETL prep for "PROJECT" and
"SUBMISSION" database tables

## Usage

``` r
read_export(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"PROJECT" and "SUBMISSION" database tables

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/Export.csv"

read_export(
    file = path
)
} # }
```
