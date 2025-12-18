# Ingest "Organization.csv" file and perform ETL prep for "PROJECT" database table

Ingest "Organization.csv" file and perform ETL prep for "PROJECT"
database table

## Usage

``` r
read_organization(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"PROJECT" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/Organization.csv"

read_organization(
    file = path
)
} # }
```
