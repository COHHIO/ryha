test_that("validate_colnames errors when colnames are missing", {
    temp_dir <- tempfile()
    dir.create(temp_dir)

    write.csv(head(iris), file.path(temp_dir, "iris.csv"))

    expect_error(validate_colnames(file.path(temp_dir, "iris.csv"), "SomeMissingCol"))
})

test_that("validate_colnames does not throw error when there are no missing columns", {
    temp_dir <- tempfile()
    dir.create(temp_dir)

    write.csv(head(iris), file.path(temp_dir, "iris.csv"))

    expect_error(validate_colnames(
        file.path(temp_dir, "iris.csv"),
        c("Sepal.Length", "Species")
    ))
})
