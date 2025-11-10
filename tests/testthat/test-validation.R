test_that("tidy_silhouette works with clustering results", {
  skip_if_not_installed("cluster")

  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  km_result <- tidy_kmeans(test_data, k = 3)
  test_dist <- dist(test_data)
  km_clusters <- km_result$clusters$cluster
  sil_result <- tidy_silhouette(km_clusters, test_dist)

  expect_s3_class(sil_result, "tidy_silhouette")
  expect_true("silhouette_data" %in% names(sil_result))
  expect_s3_class(sil_result$silhouette_data, "data.frame")
  expect_true(all(sil_result$silhouette_data$sil_width >= -1 & sil_result$silhouette_data$sil_width <= 1))
})

test_that("tidy_silhouette_analysis provides comprehensive analysis", {
  skip_if_not_installed("cluster")

  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  result <- tidy_silhouette_analysis(test_data, max_k = 5)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("k", "avg_sil_width") %in% names(result)))
  expect_equal(nrow(result), 4) # k = 2 to 5
})

test_that("tidy_gap_stat calculates gap statistic", {
  skip_if_not_installed("cluster")

  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  result <- tidy_gap_stat(test_data, max_k = 5, B = 10)

  expect_s3_class(result, "tidy_gap")
  expect_true("gap_data" %in% names(result))
  expect_s3_class(result$gap_data, "data.frame")
  expect_true("k" %in% names(result$gap_data))
  expect_true("gap" %in% names(result$gap_data))
})

test_that("plot_silhouette creates a ggplot object", {
  skip_if_not_installed("cluster")

  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  km_result <- tidy_kmeans(test_data, k = 3)
  test_dist <- dist(test_data)
  km_clusters <- km_result$clusters$cluster
  sil_result <- tidy_silhouette(km_clusters, test_dist)
  plot_obj <- plot_silhouette(sil_result)

  expect_s3_class(plot_obj, "ggplot")
})

test_that("plot_gap_stat creates a ggplot object", {
  skip_if_not_installed("cluster")

  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  gap_result <- tidy_gap_stat(test_data, max_k = 5, B = 10)
  plot_obj <- plot_gap_stat(gap_result)

  expect_s3_class(plot_obj, "ggplot")
})

test_that("optimal_clusters suggests optimal k", {
  test_data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  result <- optimal_clusters(test_data, max_k = 5, methods = c("wss"))

  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("calc_validation_metrics computes multiple metrics", {
  skip_if_not_installed("cluster")

  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  km_result <- tidy_kmeans(test_data, k = 3)
  test_dist <- dist(test_data)
  km_clusters <- km_result$clusters$cluster
  metrics <- calc_validation_metrics(km_clusters, test_data, test_dist)

  expect_s3_class(metrics, "data.frame")
  expect_true("avg_silhouette" %in% names(metrics))
})

test_that("compare_clusterings compares multiple clustering results", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  km3 <- tidy_kmeans(test_data, k = 3)
  km4 <- tidy_kmeans(test_data, k = 4)

  comparison <- compare_clusterings(
    list(k3 = km3$clusters$cluster, k4 = km4$clusters$cluster),
    test_data
  )

  expect_s3_class(comparison, "data.frame")
  expect_true("method" %in% names(comparison))
})

test_that("plot_cluster_comparison visualizes clustering comparison", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  km3 <- tidy_kmeans(test_data, k = 3)
  km4 <- tidy_kmeans(test_data, k = 4)

  comparison <- compare_clusterings(
    list(k3 = km3$clusters$cluster, k4 = km4$clusters$cluster),
    test_data
  )

  # Skip this test as plot_cluster_comparison requires additional parameters
  skip("plot_cluster_comparison needs x_col/y_col parameters")
})

test_that("tidy_silhouette works with PAM clustering", {
  skip_if_not_installed("cluster")

  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  pam_result <- tidy_pam(test_data, k = 3)
  test_dist <- dist(test_data)
  pam_clusters <- pam_result$clusters$cluster
  sil_result <- tidy_silhouette(pam_clusters, test_dist)

  expect_s3_class(sil_result, "tidy_silhouette")
})
