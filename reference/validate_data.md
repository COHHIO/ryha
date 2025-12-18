# Validate data

`validate_data()` checks whether the provided dataset meets the minimum
row requirements. It first ensures that the dataset has at least one
row, and then enforces a minimum row threshold to maintain
confidentiality. If the validation fails, a Shiny validation error is
triggered, preventing further execution.

## Usage

``` r
validate_data(data, min_n = 10)
```

## Arguments

- data:

  A data frame to validate

- min_n:

  An integer specifying the minimum number of rows required to maintain
  confidentiality. Defaults to 10.

## Value

This function does not return a value. It triggers a validation error in
a Shiny app if the dataset is empty, preventing further execution.
