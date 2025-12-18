# Ingest "EmploymentEducation.csv" file and perform ETL prep for "EMPLOYMENT" database table

Ingest "EmploymentEducation.csv" file and perform ETL prep for
"EMPLOYMENT" database table

## Usage

``` r
read_employment(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"EMPLOYMENT" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/EmploymentEducation.csv"

read_employment(
    file = path
)
} # }
```
