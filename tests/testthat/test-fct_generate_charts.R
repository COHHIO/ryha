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
