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
  calculation. When supplied, the chart is treated as a
  "share-of-denominator" chart: each bar is drawn in front of a faint
  background bar that extends to `pct_denominator`, and the value axis
  is scaled to that same maximum, so bars visually represent their share
  of a common total rather than parts of a whole.

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
