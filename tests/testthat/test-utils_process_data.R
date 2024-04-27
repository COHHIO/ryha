test_that("find_file returns correct filepath", {

  mock_filepaths <- c(
    "some/path/to/file/data1.csv",
    "some/path/to/file/data2.csv"
  )

  expect_equal(find_file(mock_filepaths, "data1"), "some/path/to/file/data1.csv")

})
