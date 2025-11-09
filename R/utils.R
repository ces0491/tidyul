#' Tidy Summary
#'
#' Create a tidy summary of clustering or dimensionality reduction results
#'
#' @param obj A tidy object (tidy_pca, tidy_kmeans, tidy_hclust, etc.)
#'
#' @return A tibble with summary statistics
#' @export
tidy_summary <- function(obj) {
  UseMethod("tidy_summary")
}

#' @export
tidy_summary.tidy_pca <- function(obj) {
  obj$variance
}

#' @export
tidy_summary.tidy_kmeans <- function(obj) {
  obj$metrics
}

#' @export
tidy_summary.tidy_pam <- function(obj) {
  tibble::tibble(
    k = length(unique(obj$clusters$cluster)),
    avg_silhouette = obj$silhouette_avg,
    n_obs = nrow(obj$clusters)
  )
}

#' @export
tidy_summary.default <- function(obj) {
  cat("No summary method available for this object type\n")
  invisible(NULL)
}


#' Null-coalescing Operator
#'
#' @param x First value
#' @param y Second value (used if x is NULL)
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


#' Check if Packages are Available
#'
#' @param packages Character vector of package names
#' @return Logical vector of same length
#' @keywords internal
check_packages <- function(packages) {
  sapply(packages, requireNamespace, quietly = TRUE)
}


#' Suggest Missing Packages
#'
#' @param packages Character vector of package names
#' @keywords internal
suggest_packages <- function(packages) {
  missing <- packages[!check_packages(packages)]
  if (length(missing) > 0) {
    message("The following packages are recommended:\n  ",
            paste(missing, collapse = ", "),
            "\nInstall with: install.packages(c('", paste(missing, collapse = "', '"), "'))")
  }
}


#' Scale to Range
#'
#' Scale numeric vector to a specific range
#'
#' @param x Numeric vector
#' @param new_min New minimum (default: 0)
#' @param new_max New maximum (default: 1)
#'
#' @return Scaled numeric vector
#' @export
scale_to_range <- function(x, new_min = 0, new_max = 1) {
  x_range <- range(x, na.rm = TRUE)
  scaled <- (x - x_range[1]) / (x_range[2] - x_range[1])
  scaled * (new_max - new_min) + new_min
}


#' Convert to Percentage
#'
#' @param x Numeric vector (proportion)
#' @param digits Number of decimal places (default: 1)
#'
#' @return Character vector with percentages
#' @export
to_percent <- function(x, digits = 1) {
  sprintf(paste0("%.", digits, "f%%"), x * 100)
}


#' Extract Cluster Centers
#'
#' Get cluster centers from various clustering objects
#'
#' @param obj A clustering object
#'
#' @return A tibble of cluster centers
#' @export
extract_centers <- function(obj) {
  UseMethod("extract_centers")
}

#' @export
extract_centers.tidy_kmeans <- function(obj) {
  obj$centers
}

#' @export
extract_centers.tidy_pam <- function(obj) {
  obj$medoids
}

#' @export
extract_centers.kmeans <- function(obj) {
  tibble::as_tibble(obj$centers) %>%
    dplyr::mutate(cluster = seq_len(nrow(obj$centers)), .before = 1)
}

#' @export
extract_centers.default <- function(obj) {
  stop("Cannot extract centers from this object type")
}


#' Quick Cluster Analysis
#'
#' Perform quick clustering with automatic k selection
#'
#' @param data A data frame or tibble
#' @param method Clustering method: "kmeans" (default), "hclust", "pam", "dbscan"
#' @param max_k Maximum k to test for automatic selection (default: 10)
#' @param ... Additional arguments passed to clustering function
#'
#' @return A clustering result object
#' @export
quick_cluster <- function(data, method = "kmeans", max_k = 10, ...) {

  message("Performing quick cluster analysis...")

  # Select optimal k using silhouette
  if (method %in% c("kmeans", "hclust", "pam")) {
    message("Finding optimal k using silhouette analysis...")
    sil_results <- tidy_silhouette_analysis(data, max_k = max_k, method = method)
    optimal_k <- attr(sil_results, "optimal_k")
    message(sprintf("Optimal k = %d", optimal_k))

    # Perform clustering with optimal k
    if (method == "kmeans") {
      result <- tidy_kmeans(data, k = optimal_k, ...)
    } else if (method == "hclust") {
      hc_result <- tidy_hclust(data, ...)
      result <- list(
        hclust_obj = hc_result,
        clusters = tidy_cutree(hc_result, k = optimal_k),
        k = optimal_k
      )
    } else if (method == "pam") {
      result <- tidy_pam(data, k = optimal_k, ...)
    }

  } else if (method == "dbscan") {
    message("Suggesting eps for DBSCAN...")
    eps_info <- suggest_eps(data, minPts = 5)
    message(sprintf("Suggested eps = %.3f", eps_info$eps))

    result <- tidy_dbscan(data, eps = eps_info$eps, minPts = 5, ...)

  } else {
    stop("method must be one of: kmeans, hclust, pam, dbscan")
  }

  message("Done!")
  result
}


#' Export Clustering Results
#'
#' Export clustering results to CSV
#'
#' @param obj A clustering object
#' @param data Original data
#' @param file File path for output
#'
#' @export
export_clusters <- function(obj, data, file) {

  # Get cluster assignments
  if (inherits(obj, "tidy_kmeans")) {
    clusters <- obj$clusters$cluster
  } else if (inherits(obj, "tidy_pam")) {
    clusters <- obj$clusters$cluster
  } else if (inherits(obj, "tidy_dbscan")) {
    clusters <- obj$clusters$cluster
  } else {
    stop("Unsupported object type")
  }

  # Combine with data
  output <- data %>%
    dplyr::mutate(cluster = clusters)

  # Write to file
  utils::write.csv(output, file = file, row.names = FALSE)
  message(sprintf("Results exported to %s", file))

  invisible(output)
}


#' Create Example Data for Testing
#'
#' Generate synthetic data for testing clustering algorithms
#'
#' @param n Number of observations per cluster (default: 100)
#' @param k Number of clusters (default: 3)
#' @param p Number of variables (default: 2)
#' @param separation Cluster separation (default: 3)
#' @param noise Proportion of noise points (default: 0, no noise)
#'
#' @return A list with data and true labels
#' @export
create_example_data <- function(n = 100, k = 3, p = 2, separation = 3, noise = 0) {

  # Generate cluster centers
  centers <- matrix(rnorm(k * p, sd = separation), nrow = k, ncol = p)

  # Generate data for each cluster
  data_list <- lapply(1:k, function(i) {
    MASS::mvrnorm(n, mu = centers[i, ], Sigma = diag(p))
  })

  # Combine data
  data_matrix <- do.call(rbind, data_list)
  true_labels <- rep(1:k, each = n)

  # Add noise if requested
  if (noise > 0) {
    n_noise <- round(n * k * noise)
    noise_data <- matrix(runif(n_noise * p, min(data_matrix) - 2, max(data_matrix) + 2),
                         nrow = n_noise, ncol = p)
    data_matrix <- rbind(data_matrix, noise_data)
    true_labels <- c(true_labels, rep(0, n_noise))  # 0 = noise
  }

  # Convert to tibble
  colnames(data_matrix) <- paste0("var", 1:p)
  data_tbl <- tibble::as_tibble(data_matrix)

  list(
    data = data_tbl,
    true_labels = true_labels,
    centers = centers
  )
}


#' Pipe Operator
#'
#' Re-export magrittr pipe operator
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL
