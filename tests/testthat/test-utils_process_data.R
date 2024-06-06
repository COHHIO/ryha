test_that("find_file returns correct filepath", {

  temp_dir <- tempdir()

  write.csv(
    data.frame(x = 1:5, y = letters[1:5]),
    file = file.path(temp_dir, "data1.csv"),
    row.names = FALSE
  )

  write.csv(
    data.frame(x = 1:5, y = letters[1:5]),
    file = file.path(temp_dir, "data2.csv"),
    row.names = FALSE
  )

  expect_equal(
    find_file(normalizePath(fs::dir_ls(temp_dir), winslash = "/"), "data1"),
    file.path(normalizePath(temp_dir, winslash = "/"), "data1.csv")
  )

})
