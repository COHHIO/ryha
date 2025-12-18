# Create a custom pickerInput

`custom_pickerInput()` is a wrapper around
[`shinyWidgets::pickerInput()`](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html)
that includes commonly used values as default.

## Usage

``` r
custom_pickerInput(
  inputId,
  label,
  choices = NULL,
  selected = NULL,
  multiple = TRUE,
  width = "100%",
  opts_actionsBox = TRUE,
  opts_selectedTextFormat = "count > 1",
  opts_liveSearch = TRUE,
  opts_container = "body",
  ...
)
```

## Arguments

- inputId:

  A character string representing the input ID

- label:

  A character string specifying the label for the picker input

- choices:

  A list of choices available for selection (default: `NULL`)

- selected:

  The initially selected value(s) (default: `NULL`)

- multiple:

  A logical indicating whether multiple selections are allowed (default:
  `TRUE`)

- width:

  A character string specifying the width of the input (default:
  `"100%"`)

- opts_actionsBox:

  A logical indicating whether to show an actions box (default: `TRUE`)

- opts_selectedTextFormat:

  A character string specifying the format of the selected text
  (default: `"count > 1"`)

- opts_liveSearch:

  A logical indicating whether to enable live search (default: `TRUE`)

- opts_container:

  A character string specifying where the dropdown should be contained
  (default: `"body"`)

- ...:

  Additional arguments passed to
  [`shinyWidgets::pickerInput()`](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html).

## Value

A `shiny.tag` object representing the picker input.
