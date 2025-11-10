test_that("tidy_pca works with basic input", {
  # Create test data
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50),
    x4 = rnorm(50)
  )

  result <- tidy_pca(test_data)

  expect_s3_class(result, "tidy_pca")
  expect_true(inherits(result, "list"))
  expect_named(result, c("scores", "loadings", "variance", "model", "settings"))
  expect_s3_class(result$scores, "data.frame")
  expect_s3_class(result$loadings, "data.frame")
  expect_s3_class(result$variance, "data.frame")
})

test_that("tidy_pca handles scale parameter", {
  test_data <- data.frame(
    x1 = rnorm(50, mean = 100, sd = 10),
    x2 = rnorm(50, mean = 5, sd = 1)
  )

  result_scaled <- tidy_pca(test_data, scale = TRUE)
  result_unscaled <- tidy_pca(test_data, scale = FALSE)

  expect_s3_class(result_scaled, "tidy_pca")
  expect_s3_class(result_unscaled, "tidy_pca")
  expect_false(identical(result_scaled$scores, result_unscaled$scores))
})

test_that("tidy_pca returns all components", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50),
    x4 = rnorm(50)
  )

  result <- tidy_pca(test_data)

  # scores has .obs_id column plus PC columns
  expect_equal(ncol(result$scores), 5) # .obs_id + 4 PCs
  expect_equal(nrow(result$variance), 4)
})

test_that("augment_pca adds PC scores to original data", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50)
  )

  pca_result <- tidy_pca(test_data)
  augmented <- augment_pca(pca_result, test_data)

  expect_s3_class(augmented, "data.frame")
  expect_true(ncol(augmented) > ncol(test_data))
  expect_true(all(names(test_data) %in% names(augmented)))
  expect_equal(nrow(augmented), nrow(test_data))
})

test_that("get_pca_loadings extracts loadings correctly", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50)
  )

  pca_result <- tidy_pca(test_data)
  loadings <- get_pca_loadings(pca_result)

  expect_s3_class(loadings, "data.frame")
  expect_true("variable" %in% names(loadings))
})

test_that("get_pca_variance extracts variance information", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50)
  )

  pca_result <- tidy_pca(test_data)
  variance <- get_pca_variance(pca_result)

  expect_s3_class(variance, "data.frame")
  expect_true(all(c("component", "variance", "prop_variance", "cum_variance") %in% names(variance)))
  expect_true(all(variance$prop_variance >= 0 & variance$prop_variance <= 1))
  expect_equal(max(variance$cum_variance), 1, tolerance = 0.01)
})

test_that("tidy_pca handles iris dataset", {
  result <- tidy_pca(iris[, 1:4])

  expect_s3_class(result, "tidy_pca")
  expect_equal(nrow(result$scores), nrow(iris))
  # loadings is in long format: 4 variables * 4 components = 16 rows
  expect_equal(nrow(result$loadings), 16)
})

test_that("tidy_pca handles data frames", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  result <- tidy_pca(test_data)
  expect_s3_class(result, "tidy_pca")
})
