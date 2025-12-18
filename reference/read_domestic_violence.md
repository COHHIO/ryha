# Ingest "HealthAndDV.csv" file and perform ETL prep for "DOMESTIC_VIOLENCE" database table

Ingest "HealthAndDV.csv" file and perform ETL prep for
"DOMESTIC_VIOLENCE" database table

## Usage

``` r
read_domestic_violence(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"DOMESTIC_VIOLENCE" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/HealthAndDV.csv"

read_domestic_violence(
    file = path
)
} # }
```
