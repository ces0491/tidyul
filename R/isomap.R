#' ISOMAP (Isometric Feature Mapping)
#'
#' Performs ISOMAP dimensionality reduction using the Rdimtools package.
#' ISOMAP extends MDS by computing distances along a neighborhood graph to
#' preserve geodesic distances. It is particularly effective for nonlinear
#' dimensionality reduction.
#'
#' @param data A data frame or tibble containing the data to analyze
#' @param dims Number of dimensions for the embedding. Default is 2
#' @param k Number of nearest neighbors for graph construction. Default is 5
#' @param distance_method Distance metric to use. Options are "euclidean",
#'   "manhattan", "minkowski", "maximum". Default is "euclidean"
#' @param scale Logical; if TRUE, scales variables to have unit variance.
#'   Default is TRUE
#' @param ... Additional arguments (currently unused)
#'
#' @return A list containing:
#'   \item{embedding}{Tibble with the low-dimensional embedding coordinates}
#'   \item{stress}{Stress value (goodness of fit)}
#'   \item{k}{Number of neighbors used}
#'   \item{dims}{Number of dimensions in the embedding}
#'   \item{call}{The matched call}
#'
#' @examples
#' \dontrun{
#' library(tidyul)
#' library(dplyr)
#'
#' # Example with iris data
#' iris_numeric <- iris %>% select(where(is.numeric))
#' isomap_result <- tidy_isomap(iris_numeric, dims = 2, k = 5)
#'
#' # View embedding
#' head(isomap_result$embedding)
#'
#' # Plot with species information
#' library(ggplot2)
#' isomap_result$embedding %>%
#'   mutate(species = iris$Species) %>%
#'   ggplot(aes(x = Dim1, y = Dim2, color = species)) +
#'   geom_point(size = 3) +
#'   labs(title = "ISOMAP Embedding")
#' }
#'
#' @export
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom stats dist
tidy_isomap <- function(data,
                        dims = 2,
                        k = 5,
                        distance_method = "euclidean",
                        scale = TRUE,
                        ...) {

  # Check if Rdimtools package is available
  if (!requireNamespace("Rdimtools", quietly = TRUE)) {
    stop("Package 'Rdimtools' is required for ISOMAP. Please install it with: install.packages('Rdimtools')")
  }

  # Convert to data frame and handle non-numeric columns
  data <- as.data.frame(data)
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]

  if (ncol(numeric_data) == 0) {
    stop("No numeric columns found in data")
  }

  if (ncol(numeric_data) < ncol(data)) {
    warning("Non-numeric columns have been removed")
  }

  n <- nrow(numeric_data)

  if (k >= n) {
    stop("k must be less than the number of observations")
  }

  if (dims >= ncol(numeric_data)) {
    warning("Number of dimensions is >= number of variables. Consider reducing dims.")
  }

  # Scale data if requested
  if (scale) {
    numeric_data <- scale(numeric_data)
  }

  # Convert to matrix (Rdimtools expects a matrix)
  data_matrix <- as.matrix(numeric_data)

  # Run ISOMAP using Rdimtools
  # Rdimtools::do.isomap expects:
  # - X: an (n x p) matrix where rows are observations
  # - ndim: target dimensionality
  # - neighbors: number of nearest neighbors

  isomap_result <- Rdimtools::do.isomap(
    X = data_matrix,
    ndim = dims,
    type = c("knn", k)
  )

  # Extract embedding
  embedding_matrix <- isomap_result$Y

  # Create embedding tibble
  embedding_cols <- paste0("Dim", 1:dims)
  embedding <- as_tibble(embedding_matrix)
  names(embedding) <- embedding_cols

  # Calculate stress (goodness of fit)
  # Compute original distances and embedding distances
  original_dist <- as.matrix(dist(data_matrix, method = distance_method))
  embedding_dist <- as.matrix(dist(embedding_matrix))

  # Calculate normalized stress
  stress <- sqrt(sum((original_dist - embedding_dist)^2) / sum(original_dist^2))

  # Create result object
  result <- list(
    embedding = embedding,
    stress = stress,
    k = k,
    dims = dims,
    call = match.call()
  )

  class(result) <- c("tidy_isomap", "list")
  return(result)
}


#' Plot ISOMAP Results
#'
#' Creates a scatter plot of ISOMAP embedding
#'
#' @param x A tidy_isomap object
#' @param color Optional vector for coloring points
#' @param size Point size. Default is 3
#' @param alpha Point transparency. Default is 0.7
#' @param ... Additional arguments passed to ggplot2::geom_point()
#'
#' @return A ggplot2 object
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal
plot.tidy_isomap <- function(x, color = NULL, size = 3, alpha = 0.7, ...) {

  if (!inherits(x, "tidy_isomap")) {
    stop("Object must be of class 'tidy_isomap'")
  }

  plot_data <- x$embedding

  if (!is.null(color)) {
    plot_data$color <- color
    p <- ggplot(plot_data, aes(x = .data$Dim1, y = .data$Dim2, color = .data$color)) +
      geom_point(size = size, alpha = alpha, ...) +
      labs(color = "Group")
  } else {
    p <- ggplot(plot_data, aes(x = .data$Dim1, y = .data$Dim2)) +
      geom_point(size = size, alpha = alpha, ...)
  }

  p <- p +
    labs(
      title = "ISOMAP Embedding",
      subtitle = sprintf("Stress = %.3f, k = %d", x$stress, x$k),
      x = "Dimension 1",
      y = "Dimension 2"
    ) +
    theme_minimal()

  return(p)
}


#' Print method for tidy_isomap
#'
#' @param x A tidy_isomap object
#' @param ... Additional arguments (currently unused)
#'
#' @export
print.tidy_isomap <- function(x, ...) {
  cat("ISOMAP (Isometric Feature Mapping)\n")
  cat("===================================\n\n")
  cat(sprintf("Number of dimensions: %d\n", x$dims))
  cat(sprintf("Number of observations: %d\n", nrow(x$embedding)))
  cat(sprintf("Number of neighbors (k): %d\n", x$k))
  cat(sprintf("Stress: %.4f\n", x$stress))
  cat("\nUse plot() to visualize the embedding\n")
  invisible(x)
}


#' Summary method for tidy_isomap
#'
#' @param object A tidy_isomap object
#' @param ... Additional arguments (currently unused)
#'
#' @export
summary.tidy_isomap <- function(object, ...) {
  cat("ISOMAP Summary\n")
  cat("==============\n\n")
  cat(sprintf("Dimensions: %d\n", object$dims))
  cat(sprintf("Observations: %d\n", nrow(object$embedding)))
  cat(sprintf("Neighbors (k): %d\n", object$k))
  cat(sprintf("Stress: %.4f\n\n", object$stress))

  invisible(object)
}
