mock_metadata <- data.frame(
  FileName = c("file1.csv", "file2.csv", "file3.csv"),
  Required = c("Y", "Y", "N")
)

test_that("check_file_names correctly identifies missing files", {

  temp_dir <- tempfile()
  dir.create(temp_dir)


  file.create(
    file.path(temp_dir, "file1.csv"),
    file.path(temp_dir, "file2.csv"),
    file.path(temp_dir, "file3.csv")
  )

  expect_equal(
    check_file_names(temp_dir, mock_metadata)$valid,
    TRUE
  )

})

test_that("check_file_names correctly identifies a single missing file", {

  temp_dir <- tempfile()
  dir.create(temp_dir)

  file.create(
    file.path(temp_dir, "file1.csv"),
    file.path(temp_dir, "file3.csv")
  )

  expect_equal(
    check_file_names(temp_dir, mock_metadata)$valid,
    FALSE
  )

  expect_equal(
    check_file_names(temp_dir, mock_metadata)$missing_file_names,
    "file2.csv"
  )

})

test_that("check_file_names correctly identifies a multiple missing files", {

  temp_dir <- tempfile()
  dir.create(temp_dir)

  file.create(
    file.path(temp_dir, "file3.csv")
  )

  expect_equal(
    check_file_names(temp_dir, mock_metadata)$valid,
    FALSE
  )

  expect_equal(
    check_file_names(temp_dir, mock_metadata)$missing_file_names,
    c("file1.csv", "file2.csv")
  )

})
