test_that("pie_chart function returns echarts object", {

  mock_data <- data.frame(
    category = c("A", "B", "C"),
    count = c(10, 20, 30)
  )

  result <- pie_chart(mock_data, category = "category", count = "count")

  expect_true("echarts4r" %in% class(result))

})

test_that("bar_chart function returns echarts object", {

  mock_data <- data.frame(
    category = c("A", "B", "C"),
    count = c(10, 20, 30)
  )

  result <- bar_chart(mock_data, x = "category", y = "count")

  expect_true("echarts4r" %in% class(result))

})

test_that("sankey_chart function returns echarts object", {

  mock_data <- data.frame(
    status_at_entry = c("A", "A", "B", "B"),
    status_at_exit = c("X", "Y", "Y", "Z"),
    n = c(10, 20, 30, 20)
  )

  result <- sankey_chart(
    data = mock_data,
    entry_status = "status_at_entry",
    exit_status = "status_at_exit",
    count = "n",
    color = "green"
  )

  expect_true("echarts4r" %in% class(result))

})

test_that("get_ids_for_sankey function returns correct IDs", {

  mock_data <- tibble::tribble(
    ~organization_id, ~personal_id, ~data_collection_stage, ~date_updated,
    1, 101, "Project start", "2023-01-01",
    1, 101, "Project exit", "2023-12-31",
    2, 102, "Project start", "2023-01-01",
    2, 102, "Project exit", "2023-12-31",
    2, 103, "Project start", "2023-02-01",
    3, 104, "Project start", "2023-02-01",
    3, 104, "Project update", "2023-12-15",
    3, 105, "Project start", "2023-01-01",
    3, 105, "Project update", "2023-06-30",
    3, 105, "Project exit", "2023-12-31"
  )

  result <- get_ids_for_sankey(mock_data)

  expected_ids <- tibble::tribble(
    ~organization_id, ~personal_id,
    1, 101,
    2, 102,
    3, 105
  )

  expect_equal(result, expected_ids)
})

test_that("prep_sankey_data function returns correct data", {

  mock_data <- tibble::tribble(
    ~organization_id, ~personal_id, ~data_collection_stage, ~date_updated, ~condition,
    1, 101, "Project start", "2023-01-01", "A",
    1, 101, "Project exit", "2023-12-31", "A",
    2, 102, "Project start", "2023-01-01", "A",
    2, 102, "Project exit", "2023-12-31", "B",
    3, 105, "Project start", "2023-01-01", "B",
    3, 105, "Project update", "2023-06-30", "B",
    3, 105, "Project exit", "2023-12-31", "A"
  )

  result <- prep_sankey_data(data = mock_data, state_var = "condition")

  expected_data <- tibble::tribble(
    ~Entry, ~Exit, ~n,
    "A (Entry)", "A (Exit)", 1,
    "A (Entry)", "B (Exit)", 1,
    "B (Entry)", "A (Exit)", 1
  )

  expect_equal(result, expected_data)
})
