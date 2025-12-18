# Ingest "Client.csv" file and perform ETL prep for "CLIENT" database table

Ingest "Client.csv" file and perform ETL prep for "CLIENT" database
table

## Usage

``` r
read_client(file)
```

## Arguments

- file:

  String, the full path to the .csv file

## Value

A data frame, containing the transformed data to be written out to the
"CLIENT" database table

## Examples

``` r
if (FALSE) { # \dontrun{

path <- "path/to/Client.csv"

read_client(
    file = path
)
} # }
```
