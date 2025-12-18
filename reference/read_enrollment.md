# Ingest "Enrollment.csv" file and perform ETL prep for "ENROLLMENT" database table

Ingest "Enrollment.csv" file and perform ETL prep for "ENROLLMENT"
database table

## Usage

``` r
read_enrollment(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"ENROLLMENT" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/Enrollment.csv"

read_enrollment(
    file = path
)
} # }
```
