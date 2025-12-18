# Create a navigation panel title with an icon

`get_nav_panel_title()` generates a navigation panel title that includes
an icon and a text label displayed below it.

## Usage

``` r
get_nav_panel_title(text, icon)
```

## Arguments

- text:

  A character string representing the title text.

- icon:

  A character string specifying the name of the icon from FontAwesome.

## Value

A
[`shiny::span`](https://rstudio.github.io/htmltools/reference/builder.html)
element containing the icon and text.

## Examples

``` r
get_nav_panel_title("Education", "book-open")
#> Error in get_nav_panel_title("Education", "book-open"): could not find function "get_nav_panel_title"
```
