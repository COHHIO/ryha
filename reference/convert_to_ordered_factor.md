# Convert vector to ordered factor

`convert_to_ordered_factor()` converts a vector to an ordered factor,
ensuring that "Missing" appears as the first level, followed by the
descriptions from a provided `codes` data frame in reverse order.

## Usage

``` r
convert_to_ordered_factor(x, codes)
```

## Arguments

- x:

  A vector to be converted into an ordered factor.

- codes:

  A data frame containing a column `Description` that defines the factor
  levels (excluding "Missing").

## Value

An ordered factor with levels starting with "Missing" and followed by
the reversed descriptions.
