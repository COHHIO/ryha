# Filter most recent enrollment per group

`filter_most_recent_enrollment_per_group()` filters a dataset to retain
only the most recent enrollment for each group, based on a series
prioritization criteria

## Usage

``` r
filter_most_recent_enrollment_per_group(data, grouping_vars)
```

## Arguments

- data:

  A data frame containing enrollment information

- grouping_vars:

  A character vector specifying the grouping variables to define groups
  within the data.

## Value

A data frame with one enrollment per group

## Details

The most recent enrollment is determined by the following prioritization
criteria, applied sequentially:

1.  Retain enrollment(s) without an exit date or with the most recent
    exit date.

2.  Retain enrollment(s) with the most recent entry date.

3.  Retain enrollment(s) with the most recent `date_updated`.

4.  Retain the enrollment with the highest `enrollment_id` as a final
    tie-breaker.

## Examples

``` r
if (FALSE) { # \dontrun{
mock_data <- tibble::tribble(
    ~organization_id, ~personal_id, ~enrollment_id, ~entry_date, ~exit_date, ~date_updated,
    1L, 1L, 1000L, "2022-01-01", "2022-12-31", "2022-12-31",
    1L, 1L, 1001L, "2023-01-01", NA, "2023-01-01"
) |>
    dplyr::mutate(
        dplyr::across(c(entry_date, exit_date, date_updated), as.Date)
    )

filter_most_recent_enrollment_per_group(mock_data, c("personal_id", "organization_id"))
} # }
```
