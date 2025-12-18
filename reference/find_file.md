# Find the complete filepath of a certain .csv file

`find_file()` finds the complete filepath of a .csv file based on the
.csv basename (i.e. the file name) in a given vector of filepaths.

## Usage

``` r
find_file(files, target)
```

## Arguments

- files:

  Character vector containing the filepaths to search through.

- target:

  String specifying the name of the .csv file (without the extension)

## Value

A character vector containing the filepath for the corresponding .csv
file.

## Examples

``` r
if (FALSE) { # \dontrun{
files <- c("some/path/to/file/data1.csv", "some/path/to/file/data2.csv")
find_file(files, "data1")
} # }
```
