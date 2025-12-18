# Generate a bar chart using echarts4r

This function generates a bar chart using the echarts4r package.

## Usage

``` r
bar_chart(
  data,
  x,
  y,
  serie_name = "# of Participants",
  pct_denominator = NULL,
  axis_flip = TRUE,
  tooltip_opts = list(confine = FALSE, extraCssText = "")
)
```

## Arguments

- data:

  A data frame containing the data to be plotted.

- x:

  A character string specifying the column name in the data frame
  representing the x-axis values.

- y:

  A character string specifying the column name in the data frame
  representing the y-axis values.

- pct_denominator:

  Optional numeric value specifying the denominator for percentage
  calculation.

- axis_flip:

  A logical value indicating whether to flip the x and y axes. Default
  is TRUE.

- tooltip_opts:

  A named list of additional tooltip options passed to echarts4r's
  `e_tooltip()`. Supports `confine` and `extraCssText` options.

## Value

A bar chart visualized using echarts4r.

## Examples

``` r
if (FALSE) { # \dontrun{
mock_data <- data.frame(x = c("A", "B", "C"), y = c(10, 20, 30))
bar_chart(
    data = mock_data,
    x = "x",
    y = "y"
)
} # }
```
