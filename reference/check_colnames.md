# Check colnames

`check_colnames()` compares a set of expected column names with the
actual column names in a .csv file and errors if any of the expected
column names is not found in the file.

## Usage

``` r
check_colnames(file, expected_colnames)
```

## Arguments

- file:

  String. Full path to a .csv file.

- expected_colnames:

  Character. Set of expected column names.

## Value

`check_colnames()` does not return any value. It either produces an
error or not, so what matters is its side effect.
