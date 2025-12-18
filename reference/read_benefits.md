# Ingest "IncomeBenefits.csv" file and perform ETL prep for "BENEFITS" database table

Ingest "IncomeBenefits.csv" file and perform ETL prep for "BENEFITS"
database table

## Usage

``` r
read_benefits(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"BENEFITS" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/IncomeBenefits.csv"

read_benefits(
    file = path
)
} # }
```
