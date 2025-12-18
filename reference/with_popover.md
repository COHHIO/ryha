# Add custom Bootstrap popover

`with_popover()` creates a Bootstrap popover containing additional
information to display alongside a given text.

## Usage

``` r
with_popover(text, content, placement = "right")
```

## Arguments

- text:

  The text to display.

- content:

  The content to display inside the popover.

- placement:

  The placement of the popover relative to the text. Defaults to
  "right".

## Value

HTML Bootstrap popover containing the specified content.

## Examples

``` r
if (FALSE) { # \dontrun{
with_popover(
    text = "Some Example Text",
    content = shiny::tagList(
        shiny::span("First description line."),
        shiny::br(),
        shiny::span("Second description line.")
    )
)
} # }
```
