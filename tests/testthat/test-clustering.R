test_that("tidy_kmeans works with basic input", {
  test_data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  result <- tidy_kmeans(test_data, k = 3)

  expect_s3_class(result, "tidy_kmeans")
  expect_true(all(c("clusters", "centers", "sizes", "model") %in% names(result)))
  expect_s3_class(result$clusters, "data.frame")
  expect_s3_class(result$centers, "data.frame")
  expect_equal(nrow(result$clusters), nrow(test_data))
  expect_equal(nrow(result$centers), 3)
})

test_that("tidy_kmeans handles nstart parameter", {
  test_data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  result <- tidy_kmeans(test_data, k = 3, nstart = 25)

  expect_s3_class(result, "tidy_kmeans")
  expect_equal(nrow(result$centers), 3)
})

test_that("augment_kmeans adds cluster assignments", {
  test_data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  km_result <- tidy_kmeans(test_data, k = 3)
  augmented <- augment_kmeans(km_result, test_data)

  expect_s3_class(augmented, "data.frame")
  expect_true("cluster" %in% names(augmented))
  expect_equal(nrow(augmented), nrow(test_data))
  expect_true(all(augmented$cluster %in% 1:3))
})

test_that("tidy_pam works correctly", {
  skip_if_not_installed("cluster")

  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  result <- tidy_pam(test_data, k = 3)

  expect_s3_class(result, "tidy_pam")
  expect_true("clusters" %in% names(result))
  expect_s3_class(result$clusters, "data.frame")
  expect_equal(nrow(result$clusters), nrow(test_data))
})

test_that("augment_pam adds cluster assignments", {
  skip_if_not_installed("cluster")

  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  pam_result <- tidy_pam(test_data, k = 3)
  augmented <- augment_pam(pam_result, test_data)

  expect_s3_class(augmented, "data.frame")
  expect_true("cluster" %in% names(augmented))
  expect_equal(nrow(augmented), nrow(test_data))
})

test_that("tidy_clara works with larger datasets", {
  skip_if_not_installed("cluster")

  test_data <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200)
  )

  result <- tidy_clara(test_data, k = 3, sampsize = 40)

  expect_s3_class(result, "tidy_clara")
  expect_true("clusters" %in% names(result))
  expect_equal(nrow(result$clusters), nrow(test_data))
})

test_that("plot_clusters creates a ggplot object", {
  test_data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  km_result <- tidy_kmeans(test_data, k = 3)
  augmented_data <- augment_kmeans(km_result, test_data)
  plot_obj <- plot_clusters(augmented_data, x = "x1", y = "x2")

  expect_s3_class(plot_obj, "ggplot")
})

test_that("tidy_kmeans works with iris data", {
  result <- tidy_kmeans(iris[, 1:4], k = 3)

  expect_s3_class(result, "tidy_kmeans")
  expect_equal(nrow(result$clusters), nrow(iris))
  expect_equal(nrow(result$centers), 3)
})

test_that("extract_centers works for kmeans objects", {
  test_data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  km_result <- tidy_kmeans(test_data, k = 3)
  centers <- extract_centers(km_result)

  expect_s3_class(centers, "data.frame")
  expect_equal(nrow(centers), 3)
})

test_that("quick_cluster provides clustering recommendations", {
  test_data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  result <- quick_cluster(test_data, max_k = 5)

  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("calc_wss calculates within-cluster sum of squares", {
  test_data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  wss_result <- calc_wss(test_data, max_k = 5)

  expect_s3_class(wss_result, "data.frame")
  expect_true(all(c("k", "tot_withinss") %in% names(wss_result)))
  expect_equal(nrow(wss_result), 5)
  expect_true(all(wss_result$tot_withinss > 0))
  expect_true(all(diff(wss_result$tot_withinss) <= 0)) # WSS should decrease
})

test_that("elbow method data can be calculated", {
  test_data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  wss_result <- calc_wss(test_data, max_k = 5)

  expect_s3_class(wss_result, "data.frame")
  expect_true("tot_withinss" %in% names(wss_result))
})
