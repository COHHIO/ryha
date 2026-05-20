# Convert vector to ordered factor

`convert_to_ordered_factor()` converts a vector to an ordered factor,
ordering by reverse `Description` from the `codes` data frame.

## Usage

``` r
convert_to_ordered_factor(x, codes, add_data_not_collected = FALSE)
```

## Arguments

- x:

  A vector to be converted into an ordered factor.

- codes:

  A data frame containing a column `Description` that defines the factor
  levels.

- add_data_not_collected:

  Logical. If `TRUE`, "Data not collected" is prepended to the factor
  levels.

## Value

An ordered factor with levels in reverse order from codes.
