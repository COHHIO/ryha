# Ingest "HealthAndDV.csv" file and perform ETL prep for "HEALTH" database table

Ingest "HealthAndDV.csv" file and perform ETL prep for "HEALTH" database
table

## Usage

``` r
read_health(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"HEALTH" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/HealthAndDV.csv"

read_health(
    file = path
)
} # }
```
