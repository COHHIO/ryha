# Ingest "EmploymentEducation.csv" file and perform ETL prep for "EDUCATION" database table

Ingest "EmploymentEducation.csv" file and perform ETL prep for
"EDUCATION" database table

## Usage

``` r
read_education(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"EDUCATION" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/EmploymentEducation.csv"

read_education(
    file = path
)
} # }
```
