test_that("tidy_autoencoder works with h2o", {
  skip_if_not_installed("h2o")

  data <- iris[, 1:4]
  result <- tidy_autoencoder(data, encoding_dim = 2, epochs = 5)

  expect_s3_class(result, "tidy_autoencoder")
  expect_true("encoding" %in% names(result))
  expect_true("reconstruction" %in% names(result))
  expect_equal(nrow(result$encoding), nrow(data))
  expect_equal(ncol(result$encoding), 2)
  expect_true(!is.na(result$reconstruction_error))
})

test_that("tidy_autoencoder handles different encoding dimensions", {
  skip_if_not_installed("h2o")

  data <- iris[, 1:4]
  result <- tidy_autoencoder(data, encoding_dim = 3, epochs = 5)

  expect_equal(ncol(result$encoding), 3)
  expect_equal(result$params$encoding_dim, 3)
})

test_that("tidy_autoencoder reconstruction has correct dimensions", {
  skip_if_not_installed("h2o")

  data <- iris[, 1:4]
  result <- tidy_autoencoder(data, encoding_dim = 2, epochs = 5)

  expect_equal(ncol(result$reconstruction), ncol(data))
  expect_equal(nrow(result$reconstruction), nrow(data))
})

test_that("tidy_autoencoder removes non-numeric columns", {
  skip_if_not_installed("h2o")

  expect_warning(
    result <- tidy_autoencoder(iris, encoding_dim = 2, epochs = 5),
    "Non-numeric columns have been removed"
  )
  expect_equal(nrow(result$encoding), nrow(iris))
})

test_that("tidy_autoencoder fails with no numeric columns", {
  skip_if_not_installed("h2o")

  data <- data.frame(a = letters[1:10], b = LETTERS[1:10])
  expect_error(tidy_autoencoder(data), "No numeric columns found")
})

test_that("print method works for tidy_autoencoder", {
  skip_if_not_installed("h2o")

  data <- iris[, 1:4]
  result <- tidy_autoencoder(data, encoding_dim = 2, epochs = 5)

  expect_output(print(result), "Autoencoder")
  expect_output(print(result), "Reconstruction error")
})

test_that("summary method works for tidy_autoencoder", {
  skip_if_not_installed("h2o")

  data <- iris[, 1:4]
  result <- tidy_autoencoder(data, encoding_dim = 2, epochs = 5)

  expect_output(summary(result), "Autoencoder Summary")
  expect_output(summary(result), "Encoding dimension")
})
