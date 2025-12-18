# Ingest "IncomeBenefits.csv" file and perform ETL prep for "INCOME" database table

Ingest "IncomeBenefits.csv" file and perform ETL prep for "INCOME"
database table

## Usage

``` r
read_income(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"INCOME" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/IncomeBenefits.csv"

read_income(
    file = path
)
} # }
```
