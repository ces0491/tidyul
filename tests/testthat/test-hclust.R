test_that("tidy_hclust works with basic input", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50)
  )

  result <- tidy_hclust(test_data)

  expect_s3_class(result, "tidy_hclust")
  expect_true("model" %in% names(result))
  expect_s3_class(result$model, "hclust")
})

test_that("tidy_hclust handles different methods", {
  test_data <- data.frame(
    x1 = rnorm(30),
    x2 = rnorm(30)
  )

  result_complete <- tidy_hclust(test_data, method = "complete")
  result_average <- tidy_hclust(test_data, method = "average")
  result_single <- tidy_hclust(test_data, method = "single")

  expect_s3_class(result_complete, "tidy_hclust")
  expect_s3_class(result_average, "tidy_hclust")
  expect_s3_class(result_single, "tidy_hclust")
})

test_that("tidy_hclust handles different distance metrics", {
  test_data <- data.frame(
    x1 = rnorm(30),
    x2 = rnorm(30)
  )

  result_euclidean <- tidy_hclust(test_data, distance = "euclidean")
  result_manhattan <- tidy_hclust(test_data, distance = "manhattan")

  expect_s3_class(result_euclidean, "tidy_hclust")
  expect_s3_class(result_manhattan, "tidy_hclust")
})

test_that("tidy_cutree cuts dendrogram at specified height or k", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  hc_result <- tidy_hclust(test_data)
  cut_result <- tidy_cutree(hc_result, k = 3)

  expect_s3_class(cut_result, "data.frame")
  expect_true("cluster" %in% names(cut_result))
  expect_equal(length(unique(cut_result$cluster)), 3)
  expect_equal(nrow(cut_result), nrow(test_data))
})

test_that("tidy_dendrogram plots dendrogram", {
  test_data <- data.frame(
    x1 = rnorm(30),
    x2 = rnorm(30)
  )

  hc_result <- tidy_hclust(test_data)

  # tidy_dendrogram plots and returns hclust invisibly
  result <- tidy_dendrogram(hc_result)
  expect_s3_class(result, "hclust")
})

test_that("augment_hclust adds cluster assignments", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  hc_result <- tidy_hclust(test_data)
  augmented <- augment_hclust(hc_result, test_data, k = 3)

  expect_s3_class(augmented, "data.frame")
  expect_true("cluster" %in% names(augmented))
  expect_equal(nrow(augmented), nrow(test_data))
  expect_true(all(names(test_data) %in% names(augmented)))
})

test_that("plot_dendrogram plots and returns hclust invisibly", {
  test_data <- data.frame(
    x1 = rnorm(30),
    x2 = rnorm(30)
  )

  hc_result <- tidy_hclust(test_data)

  # plot_dendrogram uses base R plot and returns hclust invisibly
  plot_obj <- plot_dendrogram(hc_result)
  expect_s3_class(plot_obj, "hclust")
})

test_that("optimal_hclust_k suggests optimal number of clusters", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  hc_result <- tidy_hclust(test_data)
  optimal_k <- optimal_hclust_k(hc_result, max_k = 10)

  expect_type(optimal_k, "list")
  expect_true("optimal_k" %in% names(optimal_k))
})

test_that("tidy_hclust works with iris data", {
  result <- tidy_hclust(iris[, 1:4])

  expect_s3_class(result, "tidy_hclust")
  expect_equal(length(result$model$order), nrow(iris))
})
