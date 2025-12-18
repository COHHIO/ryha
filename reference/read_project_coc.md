# Ingest "ProjectCoC.csv" file and perform ETL prep for "PROJECT COC" database table

Ingest "ProjectCoC.csv" file and perform ETL prep for "PROJECT COC"
database table

## Usage

``` r
read_project_coc(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"PROJECT COC" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/ProjectCoC.csv"

read_project_coc(
    file = path
)
} # }
```
