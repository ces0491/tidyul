test_that("tidy_dist works with basic input", {
  test_data <- data.frame(
    x1 = rnorm(30),
    x2 = rnorm(30),
    x3 = rnorm(30)
  )

  result <- tidy_dist(test_data)

  expect_s3_class(result, "dist")
  expect_equal(length(result), 30 * 29 / 2) # n * (n-1) / 2 pairwise distances
  expect_true(all(result >= 0))
})

test_that("tidy_dist handles different distance methods", {
  test_data <- data.frame(
    x1 = rnorm(20),
    x2 = rnorm(20)
  )

  result_euclidean <- tidy_dist(test_data, method = "euclidean")
  result_manhattan <- tidy_dist(test_data, method = "manhattan")
  result_maximum <- tidy_dist(test_data, method = "maximum")

  expect_s3_class(result_euclidean, "dist")
  expect_s3_class(result_manhattan, "dist")
  expect_s3_class(result_maximum, "dist")
  expect_false(identical(as.numeric(result_euclidean), as.numeric(result_manhattan)))
})

test_that("tidy_gower calculates Gower distance", {
  skip_if_not_installed("cluster")

  test_data <- data.frame(
    num1 = rnorm(30),
    num2 = rnorm(30),
    cat1 = sample(c("A", "B", "C"), 30, replace = TRUE)
  )

  result <- tidy_gower(test_data)

  expect_s3_class(result, "dist")
  expect_true(all(result >= 0 & result <= 1))
})

test_that("tidy_gower handles mixed data types", {
  skip_if_not_installed("cluster")

  test_data <- data.frame(
    numeric_var = rnorm(30),
    factor_var = factor(sample(c("A", "B", "C"), 30, replace = TRUE)),
    logical_var = sample(c(TRUE, FALSE), 30, replace = TRUE)
  )

  result <- tidy_gower(test_data)

  expect_s3_class(result, "dist")
  expect_true(all(result >= 0 & result <= 1))
})

test_that("plot_distance_heatmap creates a ggplot object", {
  test_data <- data.frame(
    x1 = rnorm(20),
    x2 = rnorm(20)
  )

  dist_result <- tidy_dist(test_data)
  plot_obj <- plot_distance_heatmap(dist_result)

  expect_s3_class(plot_obj, "ggplot")
})

test_that("compare_distances compares multiple distance metrics", {
  test_data <- data.frame(
    x1 = rnorm(30),
    x2 = rnorm(30)
  )

  result <- compare_distances(
    test_data,
    methods = c("euclidean", "manhattan")
  )

  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("tidy_dist works with iris data", {
  result <- tidy_dist(iris[1:30, 1:4])

  expect_s3_class(result, "dist")
  expect_equal(length(result), 30 * 29 / 2)
  expect_true(all(result >= 0))
})

test_that("tidy_dist preserves row names if present", {
  test_data <- data.frame(
    x1 = rnorm(10),
    x2 = rnorm(10),
    row.names = paste0("obs", 1:10)
  )

  result <- tidy_dist(test_data)

  expect_s3_class(result, "dist")
  expect_true(all(grepl("obs", attr(result, "Labels"))))
})
