test_that("tidy_isomap works with basic input", {
  skip_if_not_installed("Rdimtools")

  data <- iris[, 1:4]
  result <- tidy_isomap(data, dims = 2, k = 5)

  expect_s3_class(result, "tidy_isomap")
  expect_true("embedding" %in% names(result))
  expect_equal(nrow(result$embedding), nrow(data))
  expect_equal(ncol(result$embedding), 2)
  expect_true(!is.na(result$stress))
})

test_that("tidy_isomap handles different dimensions", {
  skip_if_not_installed("Rdimtools")

  data <- iris[, 1:4]
  result <- tidy_isomap(data, dims = 3, k = 5)

  expect_equal(ncol(result$embedding), 3)
  expect_equal(result$dims, 3)
})

test_that("tidy_isomap handles different k values", {
  skip_if_not_installed("Rdimtools")

  data <- iris[, 1:4]
  result <- tidy_isomap(data, dims = 2, k = 10)

  expect_equal(result$k, 10)
})

test_that("tidy_isomap fails when k is too large", {
  skip_if_not_installed("Rdimtools")

  data <- iris[1:20, 1:4]
  expect_error(tidy_isomap(data, k = 25), "k must be less than")
})

test_that("tidy_isomap removes non-numeric columns", {
  skip_if_not_installed("Rdimtools")

  expect_warning(
    result <- tidy_isomap(iris, dims = 2, k = 5),
    "Non-numeric columns have been removed"
  )
  expect_equal(nrow(result$embedding), nrow(iris))
})

test_that("tidy_isomap fails with no numeric columns", {
  skip_if_not_installed("Rdimtools")

  data <- data.frame(a = letters[1:10], b = LETTERS[1:10])
  expect_error(tidy_isomap(data), "No numeric columns found")
})

test_that("print method works for tidy_isomap", {
  skip_if_not_installed("Rdimtools")

  data <- iris[, 1:4]
  result <- tidy_isomap(data, dims = 2, k = 5)

  expect_output(print(result), "ISOMAP")
  expect_output(print(result), "Stress")
})

test_that("summary method works for tidy_isomap", {
  skip_if_not_installed("Rdimtools")

  data <- iris[, 1:4]
  result <- tidy_isomap(data, dims = 2, k = 5)

  expect_output(summary(result), "ISOMAP Summary")
})
