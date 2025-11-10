test_that("tidy_mds works with basic input", {
  test_data <- data.frame(
    x1 = rnorm(30),
    x2 = rnorm(30),
    x3 = rnorm(30)
  )

  result <- tidy_mds(test_data, ndim = 2)

  expect_s3_class(result, "tidy_mds")
  expect_true("config" %in% names(result))
  expect_s3_class(result$config, "data.frame")
  expect_equal(nrow(result$config), nrow(test_data))
  expect_true(ncol(result$config) >= 2) # At least 2 dimensions
})

test_that("tidy_mds_classical produces valid output", {
  test_data <- data.frame(
    x1 = rnorm(30),
    x2 = rnorm(30),
    x3 = rnorm(30)
  )
  test_dist <- dist(test_data)

  result <- tidy_mds_classical(test_dist, ndim = 2)

  expect_s3_class(result, "tidy_mds")
  expect_true("config" %in% names(result))
  expect_s3_class(result$config, "data.frame")
})

test_that("tidy_mds_smacof works with distance matrix", {
  skip_if_not_installed("smacof")

  test_data <- data.frame(
    x1 = rnorm(20),
    x2 = rnorm(20),
    x3 = rnorm(20)
  )
  test_dist <- dist(test_data)

  result <- tidy_mds_smacof(test_dist, ndim = 2)

  expect_s3_class(result, "tidy_mds")
  expect_true("config" %in% names(result))
})

test_that("tidy_mds_sammon works correctly", {
  skip_if_not_installed("MASS")

  test_data <- data.frame(
    x1 = rnorm(20),
    x2 = rnorm(20),
    x3 = rnorm(20)
  )
  test_dist <- dist(test_data)

  result <- tidy_mds_sammon(test_dist, ndim = 2)

  expect_s3_class(result, "tidy_mds")
  expect_true("config" %in% names(result))
  expect_equal(nrow(result$config), nrow(test_data))
})

test_that("tidy_mds_kruskal works correctly", {
  skip_if_not_installed("MASS")

  test_data <- data.frame(
    x1 = rnorm(20),
    x2 = rnorm(20),
    x3 = rnorm(20)
  )
  test_dist <- dist(test_data)

  result <- tidy_mds_kruskal(test_dist, ndim = 2)

  expect_s3_class(result, "tidy_mds")
  expect_true("config" %in% names(result))
})

test_that("tidy_mds handles different distance methods", {
  test_data <- data.frame(
    x1 = rnorm(30),
    x2 = rnorm(30),
    x3 = rnorm(30)
  )

  result_euclidean <- tidy_mds(test_data, ndim = 2, distance = "euclidean")
  result_manhattan <- tidy_mds(test_data, ndim = 2, distance = "manhattan")

  expect_s3_class(result_euclidean, "tidy_mds")
  expect_s3_class(result_manhattan, "tidy_mds")
})

test_that("tidy_mds works with iris data", {
  result <- tidy_mds(iris[, 1:4], ndim = 2)

  expect_s3_class(result, "tidy_mds")
  expect_equal(nrow(result$config), nrow(iris))
  expect_true(ncol(result$config) >= 2) # At least 2 dimensions
})

test_that("plot_mds creates a ggplot object", {
  test_data <- data.frame(
    x1 = rnorm(30),
    x2 = rnorm(30),
    x3 = rnorm(30)
  )

  result <- tidy_mds(test_data, ndim = 2)
  plot_obj <- plot_mds(result)

  expect_s3_class(plot_obj, "ggplot")
})
