# Ingest "CurrentLivingSituation.csv" file and perform ETL prep for "LIVING" database table

Ingest "CurrentLivingSituation.csv" file and perform ETL prep for
"LIVING" database table

## Usage

``` r
read_living(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"LIVING" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/CurrentLivingSituation.csv"

read_living(
    file = path
)
} # }
```
