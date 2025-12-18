# Generate a Sankey chart using echarts4r

This function generates a Sankey chart using the echarts4r package.

## Usage

``` r
sankey_chart(data, entry_status, exit_status, count, color = palette$default)
```

## Arguments

- data:

  A data frame containing the data to be plotted.

- entry_status:

  A character string specifying the column name in the data frame
  representing the entry status of the flow.

- exit_status:

  A character string specifying the column name in the data frame
  representing the exit status of the flow.

- count:

  A character string specifying the column name in the data frame
  representing the count or value associated with each flow.

- color:

  A character string specifying the color of the Sankey chart. Default
  is "blue".

## Value

A Sankey chart visualized using echarts4r.

## Examples

``` r
if (FALSE) { # \dontrun{
mock_data <- data.frame(
    status_at_entry = c("A", "A", "B", "B"),
    status_at_exit = c("X", "Y", "Y", "Z"),
    n = c(10, 20, 30, 20)
)
sankey_chart(
    data = mock_data,
    entry_status = "status_at_entry",
    exit_status = "status_at_exit",
    count = "n",
    color = "green"
)
} # }
```
