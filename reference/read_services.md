# Ingest "Services.csv" file and perform ETL prep for "SERVICES" database table

Ingest "Services.csv" file and perform ETL prep for "SERVICES" database
table

## Usage

``` r
read_services(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"SERVICES" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/Services.csv"

read_services(
    file = path
)
} # }
```
