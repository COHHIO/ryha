# Filter data based on clients and validate

`filter_data()` filters the provided dataset to include only records
that match the given `clients_filtered` dataset at the specified level
(enrollment or youth). It then validates the filtered dataset to ensure
it is not empty.

## Usage

``` r
filter_data(data, clients_filtered, at = "enrollment")
```

## Arguments

- data:

  A data frame to be filtered

- clients_filtered:

  A data frame containing the client records to filter `data` by. Must
  contain columns that match the selected `at` level.

- at:

  A character string specifying the filtering level. Either
  `"enrollment"` (default) or `"youth"`.

  - `"enrollment"`: Filters data based on `personal_id`,
    `organization_id` and `enrollment_id`.

  - `"youth"`: Filters data based on `personal_id` and
    `organization_id`.

## Value

A filtered data frame containing only the records matching the specified
clients. If no matching records are found, a validation error is
triggered.
