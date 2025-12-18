# Prepare data for a Sankey chart

`prepare_sankey_data()` prepares data for visualization in a Sankey
chart.

## Usage

``` r
prepare_sankey_data(data, response_col, response_vals)
```

## Arguments

- data:

  A data frame

- response_col:

  A character string specifying the name of the column with response
  values

- response_vals:

  A vector of response values to include in the Sankey chart

## Value

A data frame summarizing the transitions between entry and exit response
values, with columns `Entry`, `Exit`, and `n` representing the start,
end and count of each transition.

## Examples

``` r
if (FALSE) { # \dontrun{
mock_data <- data.frame(
    enrollment_id = rep(1:5, each = 2),
    personal_id = rep(1:5, each = 2),
    organization_id = 1,
    data_collection_stage = rep(c("Project start", "Project exit"), 5),
    status = c("A", "B", "A", "B", "B", "C", "A", "C", "A", "D")
)

prepare_sankey_data(mock_data, response_col = "status", response_vals = c("A", "B", "C"))
} # }
```
