test_that("standardize_data standardizes numeric data", {
  test_data <- data.frame(
    x1 = rnorm(50, mean = 100, sd = 15),
    x2 = rnorm(50, mean = 5, sd = 2),
    x3 = rnorm(50, mean = 0, sd = 1)
  )

  result <- standardize_data(test_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(test_data))
  expect_equal(ncol(result), ncol(test_data))

  # Check that means are approximately 0
  expect_true(all(abs(colMeans(result)) < 0.1))

  # Check that standard deviations are approximately 1
  expect_true(all(abs(apply(result, 2, sd) - 1) < 0.1))
})

test_that("standardize_data handles single column", {
  test_data <- data.frame(x = rnorm(50, mean = 10, sd = 2))

  result <- standardize_data(test_data)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 1)
  expect_true(abs(mean(result$x)) < 0.1)
})

test_that("scale_to_range scales data to specified range", {
  test_vector <- rnorm(50)

  result <- scale_to_range(test_vector, new_min = 0, new_max = 1)

  expect_type(result, "double")
  expect_true(all(result >= 0 & result <= 1))
  expect_true(any(result < 0.1)) # Should have values near 0
  expect_true(any(result > 0.9)) # Should have values near 1
})

test_that("scale_to_range handles different ranges", {
  test_vector <- 1:100

  result <- scale_to_range(test_vector, new_min = -1, new_max = 1)

  expect_true(all(result >= -1 & result <= 1))
  expect_equal(min(result), -1, tolerance = 0.01)
  expect_equal(max(result), 1, tolerance = 0.01)
})

test_that("to_percent converts proportions to percentages", {
  proportions <- c(0.25, 0.50, 0.75, 1.00)

  result <- to_percent(proportions)

  expect_type(result, "character")
  expect_equal(length(result), length(proportions))
  expect_true(all(grepl("%", result)))
})

test_that("to_percent handles digits parameter", {
  proportions <- c(0.12345, 0.67890)

  result1 <- to_percent(proportions, digits = 1)
  result2 <- to_percent(proportions, digits = 3)

  expect_true(grepl("12.3%", result1[1]))
  expect_true(grepl("12.345%", result2[1]))
})

test_that("tidy_summary works with tidy objects", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  km_result <- tidy_kmeans(test_data, k = 3)
  summary_result <- tidy_summary(km_result)

  expect_type(summary_result, "list")
  expect_true(length(summary_result) > 0)
})

test_that("create_example_data generates synthetic data", {
  result <- create_example_data(n = 100, k = 3, p = 2)

  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_true("true_labels" %in% names(result))
  expect_s3_class(result$data, "data.frame")
  expect_equal(nrow(result$data), 300) # n * k observations
  expect_equal(ncol(result$data), 2)
})

test_that("create_example_data includes cluster labels", {
  result <- create_example_data(n = 100, k = 3, p = 2)

  expect_true("true_labels" %in% names(result))
  expect_equal(length(unique(result$true_labels)), 3)
  expect_equal(length(result$true_labels), 300)
})

test_that("export_clusters exports clustering results", {
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )

  km_result <- tidy_kmeans(test_data, k = 3)

  temp_file <- tempfile(fileext = ".csv")
  export_clusters(km_result, test_data, file = temp_file)

  expect_true(file.exists(temp_file))

  # Read back and verify
  exported <- read.csv(temp_file)
  expect_true("cluster" %in% names(exported))
  expect_equal(nrow(exported), nrow(test_data))

  # Clean up
  unlink(temp_file)
})

test_that("plot_cluster_sizes creates bar plot", {
  test_data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  km_result <- tidy_kmeans(test_data, k = 3)
  clusters <- km_result$clusters$cluster
  plot_obj <- plot_cluster_sizes(clusters)

  expect_s3_class(plot_obj, "ggplot")
})

test_that("create_cluster_dashboard creates comprehensive dashboard", {
  test_data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  km_result <- tidy_kmeans(test_data, k = 3)
  augmented_data <- augment_kmeans(km_result, test_data)
  dashboard <- create_cluster_dashboard(augmented_data, cluster_col = "cluster")

  expect_type(dashboard, "list")
  expect_true(length(dashboard) > 0)
})

test_that("standardize_data preserves row and column names", {
  test_data <- data.frame(
    x1 = rnorm(10),
    x2 = rnorm(10),
    row.names = paste0("obs", 1:10)
  )

  result <- standardize_data(test_data)

  expect_equal(rownames(result), rownames(test_data))
  expect_equal(names(result), names(test_data))
})
