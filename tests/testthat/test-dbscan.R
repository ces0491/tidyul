test_that("tidy_dbscan works with basic input", {
  skip_if_not_installed("dbscan")

  # Create test data with clear clusters
  set.seed(123)
  test_data <- data.frame(
    x = c(rnorm(30, 0, 0.3), rnorm(30, 3, 0.3)),
    y = c(rnorm(30, 0, 0.3), rnorm(30, 3, 0.3))
  )

  result <- tidy_dbscan(test_data, eps = 0.5, minPts = 5)

  expect_s3_class(result, "tidy_dbscan")
  expect_true("clusters" %in% names(result))
  expect_s3_class(result$clusters, "data.frame")
  expect_equal(nrow(result$clusters), nrow(test_data))
  expect_true("cluster" %in% names(result$clusters))
})

test_that("tidy_dbscan identifies noise points", {
  skip_if_not_installed("dbscan")

  set.seed(123)
  # Create data with clear outliers
  test_data <- data.frame(
    x = c(rnorm(30, 0, 0.3), rnorm(5, 10, 0.1)),
    y = c(rnorm(30, 0, 0.3), rnorm(5, 10, 0.1))
  )

  result <- tidy_dbscan(test_data, eps = 1.0, minPts = 10)

  # Check that noise detection works (some points may be noise)
  expect_true(all(result$clusters$is_noise == (result$clusters$cluster == 0)))
})

test_that("tidy_knn_dist calculates k-NN distances", {
  skip_if_not_installed("dbscan")

  test_data <- data.frame(
    x = rnorm(50),
    y = rnorm(50)
  )

  result <- tidy_knn_dist(test_data, k = 5)

  expect_s3_class(result, "data.frame")
  expect_true("knn_dist" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))
  expect_true(all(result$knn_dist >= 0))
})

test_that("suggest_eps provides epsilon recommendation", {
  skip_if_not_installed("dbscan")

  test_data <- data.frame(
    x = rnorm(50),
    y = rnorm(50)
  )

  suggested_eps <- suggest_eps(test_data, minPts = 5)

  expect_type(suggested_eps, "list")
  expect_true("eps" %in% names(suggested_eps))
  expect_true(suggested_eps$eps > 0)
})

test_that("augment_dbscan adds cluster assignments", {
  skip_if_not_installed("dbscan")

  set.seed(123)
  test_data <- data.frame(
    x = c(rnorm(30, 0, 0.3), rnorm(30, 3, 0.3)),
    y = c(rnorm(30, 0, 0.3), rnorm(30, 3, 0.3))
  )

  db_result <- tidy_dbscan(test_data, eps = 0.5, minPts = 5)
  augmented <- augment_dbscan(db_result, test_data)

  expect_s3_class(augmented, "data.frame")
  expect_true("cluster" %in% names(augmented))
  expect_equal(nrow(augmented), nrow(test_data))
  expect_true(all(names(test_data) %in% names(augmented)))
})

test_that("plot_knn_dist creates a ggplot object", {
  skip_if_not_installed("dbscan")

  test_data <- data.frame(
    x = rnorm(50),
    y = rnorm(50)
  )

  knn_result <- tidy_knn_dist(test_data, k = 5)
  plot_obj <- plot_knn_dist(knn_result)

  expect_s3_class(plot_obj, "ggplot")
})

test_that("explore_dbscan_params explores parameter space", {
  skip_if_not_installed("dbscan")

  test_data <- data.frame(
    x = rnorm(50),
    y = rnorm(50)
  )

  result <- explore_dbscan_params(
    test_data,
    eps_values = c(0.3, 0.5, 0.7),
    minPts_values = c(3, 5, 7)
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("eps", "minPts", "n_clusters") %in% names(result)))
})

test_that("tidy_dbscan handles different distance metrics", {
  skip_if_not_installed("dbscan")

  set.seed(123)
  test_data <- data.frame(
    x = rnorm(30),
    y = rnorm(30)
  )

  result <- tidy_dbscan(test_data, eps = 1, minPts = 5)

  expect_s3_class(result, "tidy_dbscan")
})
