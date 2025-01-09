test_that("filter works when there is only a 'Project start'", {
  mock_data <- tibble::tribble(
     ~test_row, ~organization_id, ~personal_id, ~enrollment_id,  ~data_collection_stage,  ~date_updated,
     1, 1L, 1L, 1000L, "Project start", "2022-12-31",
  ) |>
  dplyr::mutate(dplyr::across(date_updated, as.Date))

  kept_data_collection_stage <- filter_most_recent_data_per_enrollment(mock_data) |> 
    dplyr::pull(test_row)

  expect_equal(kept_data_collection_stage, 1)
})

test_that("filter works when 'Project start' has date_updated greater than 'Project update' date_updated", {
  mock_data <- tibble::tribble(
    ~test_row, ~organization_id, ~personal_id, ~enrollment_id,  ~data_collection_stage,  ~date_updated,
    1, 1L, 1L, 1000L, "Project start", "2024-12-31",
    2, 1L, 1L, 1000L, "Project update", "2023-01-01"
  ) |>
  dplyr::mutate(dplyr::across(date_updated, as.Date))

  kept_data_collection_stage <- filter_most_recent_data_per_enrollment(mock_data) |> 
    dplyr::pull(test_row)

  expect_equal(kept_data_collection_stage, 2)
})

test_that("filter works when 'Project exit' has the lowest date_updated", {
  mock_data <- tibble::tribble(
    ~test_row, ~organization_id, ~personal_id, ~enrollment_id,  ~data_collection_stage,  ~date_updated,
    1, 1L, 1L, 1000L, "Project start", "2022-12-31",
    2, 1L, 1L, 1000L, "Project update", "2023-01-01",
    3, 1L, 1L, 1000L, "Project exit", "2020-01-01"
  ) |>
  dplyr::mutate(dplyr::across(date_updated, as.Date))

  kept_data_collection_stage <- filter_most_recent_data_per_enrollment(mock_data) |> 
    dplyr::pull(test_row)

  expect_equal(kept_data_collection_stage, 3)
})

test_that("filter works with multiple enrollments", {
  mock_data <- tibble::tribble(
    ~test_row, ~organization_id, ~personal_id, ~enrollment_id,  ~data_collection_stage,  ~date_updated,
    1, 1L, 1L, 1000L, "Project start", "2022-12-31",
    2, 1L, 1L, 1000L, "Project update", "2023-01-01",
    3, 1L, 1L, 1000L, "Project exit", "2020-01-01",
    4, 1L, 2L, 1000L, "Project start", "2024-12-31",
    5, 1L, 2L, 1000L, "Project update", "2023-01-01"
  ) |>
  dplyr::mutate(dplyr::across(date_updated, as.Date))

  kept_data_collection_stage <- filter_most_recent_data_per_enrollment(mock_data) |> 
    dplyr::pull(test_row)

  expect_equal(kept_data_collection_stage, c(3, 5))
})

test_that("filter works with multiple 'Project update' that have the same date_updated", {
  mock_data <- tibble::tribble(
    ~test_row, ~organization_id, ~personal_id, ~enrollment_id,  ~data_collection_stage,  ~date_updated,
    1, 1L, 1L, 1000L, "Project start", "2022-12-31",
    2, 1L, 1L, 1000L, "Project update", "2023-01-01",
    3, 1L, 1L, 1000L, "Project update", "2023-01-01"
  ) |>
  dplyr::mutate(dplyr::across(date_updated, as.Date))

  kept_data_collection_stage <- filter_most_recent_data_per_enrollment(mock_data) |> 
    dplyr::pull(test_row)

  expect_equal(kept_data_collection_stage, 2)
})
