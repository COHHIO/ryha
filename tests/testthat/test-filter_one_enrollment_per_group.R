test_that("filter_one_enrollment_per_group keeps the enrollment without exit date", {
  mock_data <- tibble::tribble(
    ~organization_id, ~personal_id, ~enrollment_id,  ~entry_date,   ~exit_date, ~date_updated,
                  1L,           1L,          1000L, "2022-01-01", "2022-12-31",  "2022-12-31",
                  1L,           1L,          1001L, "2023-01-01",           NA,  "2023-01-01"
  ) |>
  dplyr::mutate(
    dplyr::across(c(entry_date, exit_date, date_updated), as.Date)
  )

  kept_enrollment <- filter_one_enrollment_per_group(mock_data, grouping_vars = c("organization_id", "personal_id")) |> 
    dplyr::pull(enrollment_id)

  expect_equal(kept_enrollment, 1001)

})

test_that("filter_one_enrollment_per_group keeps the enrollment with the most recent exit date", {
  mock_data <- tibble::tribble(
    ~organization_id, ~personal_id, ~enrollment_id,  ~entry_date,   ~exit_date, ~date_updated,
                  1L,           1L,          1000L, "2022-01-01", "2022-12-31",  "2022-12-31",
                  1L,           1L,          1001L, "2023-01-01", "2023-12-31",  "2023-12-01"
  ) |>
  dplyr::mutate(
    dplyr::across(c(entry_date, exit_date, date_updated), as.Date)
  )

  kept_enrollment <- filter_one_enrollment_per_group(mock_data, grouping_vars = c("organization_id", "personal_id")) |> 
    dplyr::pull(enrollment_id)

  expect_equal(kept_enrollment, 1001)

})

test_that("filter_one_enrollment_per_group keeps the enrollment with the most recent entry date", {
  mock_data <- tibble::tribble(
    ~organization_id, ~personal_id, ~enrollment_id,  ~entry_date,   ~exit_date, ~date_updated,
                  1L,           1L,          1000L, "2022-01-01",           NA,  "2022-12-31",
                  1L,           1L,          1001L, "2023-01-01",           NA,  "2023-12-31"
  ) |>
  dplyr::mutate(
    dplyr::across(c(entry_date, exit_date, date_updated), as.Date)
  )

  kept_enrollment <- filter_one_enrollment_per_group(mock_data, grouping_vars = c("organization_id", "personal_id")) |> 
    dplyr::pull(enrollment_id)

  expect_equal(kept_enrollment, 1001)

})

test_that("filter_one_enrollment_per_group keeps the enrollment with the most recent date_updated", {
  mock_data <- tibble::tribble(
    ~organization_id, ~personal_id, ~enrollment_id,  ~entry_date,   ~exit_date, ~date_updated,
                  1L,           1L,          1000L, "2022-01-01",           NA,  "2022-12-31",
                  1L,           1L,          1001L, "2022-01-01",           NA,  "2023-12-31"
  ) |>
  dplyr::mutate(
    dplyr::across(c(entry_date, exit_date, date_updated), as.Date)
  )

  kept_enrollment <- filter_one_enrollment_per_group(mock_data, grouping_vars = c("organization_id", "personal_id")) |> 
    dplyr::pull(enrollment_id)

  expect_equal(kept_enrollment, 1001)

})

test_that("filter_one_enrollment_per_group keeps the enrollment with the greater enrollment_id", {
  mock_data <- tibble::tribble(
    ~organization_id, ~personal_id, ~enrollment_id,  ~entry_date,   ~exit_date, ~date_updated,
                  1L,           1L,          1000L, "2022-01-01",           NA,  "2022-12-31",
                  1L,           1L,          1001L, "2022-01-01",           NA,  "2022-12-31"
  ) |>
  dplyr::mutate(
    dplyr::across(c(entry_date, exit_date, date_updated), as.Date)
  )

  kept_enrollment <- filter_one_enrollment_per_group(mock_data, grouping_vars = c("organization_id", "personal_id")) |> 
    dplyr::pull(enrollment_id)

  expect_equal(kept_enrollment, 1001)

})
