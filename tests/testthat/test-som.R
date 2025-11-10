test_that("tidy_som works with basic input", {
  skip_if_not_installed("kohonen")

  data <- iris[, 1:4]
  result <- tidy_som(data, grid_dim = c(3, 3), rlen = 10)

  expect_s3_class(result, "tidy_som")
  expect_true("mapping" %in% names(result))
  expect_true("codes" %in% names(result))
  expect_equal(nrow(result$mapping), nrow(data))
  expect_equal(result$grid$n_units, 9)
})

test_that("tidy_som handles different grid dimensions", {
  skip_if_not_installed("kohonen")

  data <- iris[, 1:4]
  result <- tidy_som(data, grid_dim = c(5, 4), rlen = 10)

  expect_equal(result$grid$dim, c(5, 4))
  expect_equal(result$grid$n_units, 20)
})

test_that("tidy_som removes non-numeric columns", {
  skip_if_not_installed("kohonen")

  expect_warning(
    result <- tidy_som(iris, grid_dim = c(3, 3), rlen = 10),
    "Non-numeric columns have been removed"
  )
  expect_equal(ncol(result$codes) - 3, 4)  # 4 numeric columns from iris
})

test_that("tidy_som fails with no numeric columns", {
  skip_if_not_installed("kohonen")

  data <- data.frame(a = letters[1:10], b = LETTERS[1:10])
  expect_error(tidy_som(data), "No numeric columns found")
})

test_that("print method works for tidy_som", {
  skip_if_not_installed("kohonen")

  data <- iris[, 1:4]
  result <- tidy_som(data, grid_dim = c(3, 3), rlen = 10)

  expect_output(print(result), "Self-Organizing Map")
  expect_output(print(result), "Grid dimensions")
})
