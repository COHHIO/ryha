# Ingest "Disabilities.csv" file and perform ETL prep for "DISABILITIES" database table

Ingest "Disabilities.csv" file and perform ETL prep for "DISABILITIES"
database table

## Usage

``` r
read_disabilities(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"DISABILITIES" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/Disabilities.csv"

read_disabilities(
    file = path
)
} # }
```
