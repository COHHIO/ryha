# Ingest "Exit.csv" file and perform ETL prep for "EXIT" database table

Ingest "Exit.csv" file and perform ETL prep for "EXIT" database table

## Usage

``` r
read_exit(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"EXIT" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/Exit.csv"

read_exit(
    file = path
)
} # }
```
