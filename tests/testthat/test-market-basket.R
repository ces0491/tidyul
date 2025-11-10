test_that("tidy_apriori works with transaction data", {
  skip_if_not_installed("arules")

  # Create sample transaction data
  trans_df <- data.frame(
    transaction_id = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5),
    item = c("bread", "milk", "eggs", "bread", "butter", "milk", "eggs",
             "cheese", "bread", "bread", "milk", "eggs", "milk", "cheese")
  )

  # Convert to transactions format
  trans_list <- split(trans_df$item, trans_df$transaction_id)
  transactions <- methods::as(trans_list, "transactions")

  result <- tidy_apriori(
    transactions,
    support = 0.2,
    confidence = 0.5
  )

  expect_s3_class(result, "tidy_apriori")
  expect_true("rules_tbl" %in% names(result))
})

test_that("tidy_rules extracts rules in tidy format", {
  skip_if_not_installed("arules")

  trans_df <- data.frame(
    transaction_id = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5),
    item = c("bread", "milk", "eggs", "bread", "butter", "milk", "eggs",
             "cheese", "bread", "bread", "milk", "eggs", "milk", "cheese")
  )

  trans_list <- split(trans_df$item, trans_df$transaction_id)
  transactions <- methods::as(trans_list, "transactions")

  apriori_result <- tidy_apriori(
    transactions,
    support = 0.2,
    confidence = 0.5
  )

  rules <- tidy_rules(apriori_result$rules)

  expect_s3_class(rules, "data.frame")
  expect_true(all(c("lhs", "rhs", "support", "confidence", "lift") %in% names(rules)))
})

test_that("inspect_rules filters rules correctly", {
  skip_if_not_installed("arules")

  trans_df <- data.frame(
    transaction_id = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5),
    item = c("bread", "milk", "eggs", "bread", "butter", "milk", "eggs",
             "cheese", "bread", "bread", "milk", "eggs", "milk", "cheese")
  )

  trans_list <- split(trans_df$item, trans_df$transaction_id)
  transactions <- methods::as(trans_list, "transactions")

  apriori_result <- tidy_apriori(
    transactions,
    support = 0.2,
    confidence = 0.5
  )

  filtered <- inspect_rules(apriori_result, by = "lift", n = 10)

  expect_s3_class(filtered, "data.frame")
  if (nrow(filtered) > 0) {
    expect_true(all(filtered$lift >= 0))
  }
})

test_that("filter_rules_by_item filters by specific item", {
  skip_if_not_installed("arules")

  trans_df <- data.frame(
    transaction_id = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5),
    item = c("bread", "milk", "eggs", "bread", "butter", "milk", "eggs",
             "cheese", "bread", "bread", "milk", "eggs", "milk", "cheese")
  )

  trans_list <- split(trans_df$item, trans_df$transaction_id)
  transactions <- methods::as(trans_list, "transactions")

  apriori_result <- tidy_apriori(
    transactions,
    support = 0.2,
    confidence = 0.5
  )

  filtered <- filter_rules_by_item(apriori_result, item = "milk")

  expect_s3_class(filtered, "data.frame")
})

test_that("recommend_products generates recommendations", {
  skip_if_not_installed("arules")

  trans_df <- data.frame(
    transaction_id = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5),
    item = c("bread", "milk", "eggs", "bread", "butter", "milk", "eggs",
             "cheese", "bread", "bread", "milk", "eggs", "milk", "cheese")
  )

  trans_list <- split(trans_df$item, trans_df$transaction_id)
  transactions <- methods::as(trans_list, "transactions")

  apriori_result <- tidy_apriori(
    transactions,
    support = 0.2,
    confidence = 0.5
  )

  recommendations <- recommend_products(apriori_result, basket = c("bread"))

  expect_s3_class(recommendations, "data.frame")
  expect_true("rhs" %in% names(recommendations))
})

test_that("find_related_items finds item associations", {
  skip_if_not_installed("arules")

  trans_df <- data.frame(
    transaction_id = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5),
    item = c("bread", "milk", "eggs", "bread", "butter", "milk", "eggs",
             "cheese", "bread", "bread", "milk", "eggs", "milk", "cheese")
  )

  trans_list <- split(trans_df$item, trans_df$transaction_id)
  transactions <- methods::as(trans_list, "transactions")

  apriori_result <- tidy_apriori(
    transactions,
    support = 0.2,
    confidence = 0.5
  )

  related <- find_related_items(apriori_result, item = "bread")

  expect_s3_class(related, "data.frame")
})

test_that("summarize_rules provides rule summary", {
  skip_if_not_installed("arules")

  trans_df <- data.frame(
    transaction_id = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5),
    item = c("bread", "milk", "eggs", "bread", "butter", "milk", "eggs",
             "cheese", "bread", "bread", "milk", "eggs", "milk", "cheese")
  )

  trans_list <- split(trans_df$item, trans_df$transaction_id)
  transactions <- methods::as(trans_list, "transactions")

  apriori_result <- tidy_apriori(
    transactions,
    support = 0.2,
    confidence = 0.5
  )

  summary_result <- summarize_rules(apriori_result)

  expect_type(summary_result, "list")
  expect_true("n_rules" %in% names(summary_result))
})

test_that("visualize_rules creates visualization", {
  skip_if_not_installed("arules")
  skip_if_not_installed("arulesViz")

  trans_df <- data.frame(
    transaction_id = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5),
    item = c("bread", "milk", "eggs", "bread", "butter", "milk", "eggs",
             "cheese", "bread", "bread", "milk", "eggs", "milk", "cheese")
  )

  trans_list <- split(trans_df$item, trans_df$transaction_id)
  transactions <- methods::as(trans_list, "transactions")

  apriori_result <- tidy_apriori(
    transactions,
    support = 0.2,
    confidence = 0.5
  )

  # Just test that it doesn't error
  expect_error(visualize_rules(apriori_result), NA)
})
