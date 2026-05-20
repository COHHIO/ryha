# Validate file data

`validate_file_data()` checks whether the provided data frame is valid
and aborts with an informative error message if validation fails.

## Usage

``` r
validate_file_data(filename, data)
```

## Arguments

- filename:

  String. Name of the file (used in error messages).

- data:

  A data frame

## Value

`validate_file_data()` is called for its side effects. It produces an
error if validation fails, otherwise nothing.
