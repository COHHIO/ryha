# Ensure All HMIS Data was Uploaded

Ensure All HMIS Data was Uploaded

## Usage

``` r
validate_file_names(dir, metadata)
```

## Arguments

- dir:

  The directory path containing the extracted files from the uploaded
  .zip file.

- metadata:

  Data frame used to identify required files. It expects the following
  columns: `FileName` (including file extension) and `Required` (either
  `Y` or `N`).

## Value

A list with two elements:

- `valid`: A logical value indicating whether all expected files are
  present in the uploaded directory.

- `missing_file_names`: A character vector containing the names of files
  that are expected but missing in the uploaded directory.

## Details

Compare the file names within the uploaded .zip file to the expected
file names (stored in the 'HMISmetadata' data object within this R
package)
