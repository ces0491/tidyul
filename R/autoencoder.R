#' Autoencoder for Dimensionality Reduction
#'
#' Trains a neural network autoencoder for nonlinear dimensionality reduction using H2O.
#' An autoencoder learns to compress data into a lower-dimensional representation
#' (encoding) and then reconstruct it (decoding).
#'
#' @param data A data frame or tibble containing the data to analyze
#' @param encoding_dim Dimension of the encoding layer (bottleneck). Default is 2
#' @param hidden_layers Vector specifying the number of units in hidden layers
#'   between input and encoding. Default is c(64, 32). The decoder mirrors this.
#' @param activation Activation function for hidden layers. Options are "Rectifier",
#'   "Tanh", "Maxout", "ExpRectifier". Default is "Tanh"
#' @param epochs Number of training epochs. Default is 100
#' @param learning_rate Learning rate for optimizer. Default is 0.001
#' @param scale Logical; if TRUE, scales variables to the range \[0, 1\]. Default is TRUE
#' @param seed Random seed for reproducibility. Default is NULL
#' @param ... Additional arguments passed to h2o.deeplearning()
#'
#' @return A list containing:
#'   \item{encoding}{Tibble with the low-dimensional encoding}
#'   \item{reconstruction}{Tibble with reconstructed data}
#'   \item{model}{The trained H2O autoencoder model}
#'   \item{reconstruction_error}{Mean squared error of reconstruction}
#'   \item{params}{List of parameters used}
#'   \item{call}{The matched call}
#'
#' @examples
#' \dontrun{
#' library(tidyul)
#' library(dplyr)
#'
#' # Example with iris data
#' iris_numeric <- iris %>% select(where(is.numeric))
#' ae_result <- tidy_autoencoder(
#'   iris_numeric,
#'   encoding_dim = 2,
#'   hidden_layers = c(8, 4),
#'   epochs = 50
#' )
#'
#' # View encoding
#' head(ae_result$encoding)
#'
#' # Check reconstruction error
#' ae_result$reconstruction_error
#'
#' # Plot encoding
#' library(ggplot2)
#' ae_result$encoding %>%
#'   mutate(species = iris$Species) %>%
#'   ggplot(aes(x = Dim1, y = Dim2, color = species)) +
#'   geom_point(size = 3)
#' }
#'
#' @export
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom stats runif
tidy_autoencoder <- function(data,
                              encoding_dim = 2,
                              hidden_layers = c(64, 32),
                              activation = "Tanh",
                              epochs = 100,
                              learning_rate = 0.001,
                              scale = TRUE,
                              seed = NULL,
                              ...) {

  # Check if h2o package is available
  if (!requireNamespace("h2o", quietly = TRUE)) {
    stop("Package 'h2o' is required for autoencoder analysis. Please install it with: install.packages('h2o')")
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

  input_dim <- ncol(numeric_data)
  n_obs <- nrow(numeric_data)
  col_names <- names(numeric_data)

  # Scale data if requested
  if (scale) {
    data_min <- apply(numeric_data, 2, min, na.rm = TRUE)
    data_max <- apply(numeric_data, 2, max, na.rm = TRUE)
    data_range <- data_max - data_min
    data_range[data_range == 0] <- 1  # Avoid division by zero
    data_scaled <- scale(numeric_data, center = data_min, scale = data_range)
    data_scaled <- as.data.frame(data_scaled)
  } else {
    data_scaled <- numeric_data
  }

  # Initialize H2O
  h2o_initialized <- tryCatch({
    h2o::h2o.getConnection()
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (!h2o_initialized) {
    message("Initializing H2O...")
    # Try with smaller memory allocation first (512MB)
    # This is more compatible with systems with limited memory
    init_result <- tryCatch({
      h2o::h2o.init(max_mem_size = "512m", nthreads = -1)
      h2o::h2o.no_progress()
      TRUE
    }, error = function(e) {
      # If 512MB fails, provide helpful error message
      stop(paste("Failed to initialize H2O. This may be due to insufficient memory or",
                 "incompatible Java version. Error:", e$message,
                 "\nTry installing 64-bit Java or increasing available system memory."))
    })
  }

  # Convert to H2O frame
  h2o_data <- h2o::as.h2o(data_scaled)

  # Build autoencoder architecture
  # Encoder layers + bottleneck + decoder layers (mirrored)
  full_hidden <- c(hidden_layers, encoding_dim, rev(hidden_layers))

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Train autoencoder with stability settings
  model <- h2o::h2o.deeplearning(
    x = names(h2o_data),
    training_frame = h2o_data,
    autoencoder = TRUE,
    hidden = full_hidden,
    epochs = epochs,
    activation = activation,
    adaptive_rate = FALSE,
    rate = learning_rate,
    l1 = 1e-5,
    l2 = 1e-5,
    max_w2 = 10,
    seed = seed,
    reproducible = !is.null(seed),
    ...
  )

  # Extract the encoding (features from the bottleneck layer)
  # This requires getting the deep features at the bottleneck layer
  encoding_layer_index <- length(hidden_layers) + 1
  encoding_h2o <- h2o::h2o.deepfeatures(model, h2o_data, layer = encoding_layer_index)
  encoding_matrix <- as.data.frame(encoding_h2o)

  # Get reconstruction
  reconstruction_h2o <- h2o::h2o.predict(model, h2o_data)
  reconstruction_matrix <- as.data.frame(reconstruction_h2o)

  # Create result tibbles
  encoding_cols <- paste0("Dim", 1:encoding_dim)
  encoding <- as_tibble(encoding_matrix)
  names(encoding) <- encoding_cols

  reconstruction <- as_tibble(reconstruction_matrix)
  names(reconstruction) <- col_names

  # Rescale reconstruction if data was scaled
  if (scale) {
    for (i in seq_along(col_names)) {
      reconstruction[[i]] <- reconstruction[[i]] * data_range[i] + data_min[i]
    }
  }

  # Calculate reconstruction error
  mse <- mean((as.matrix(numeric_data) - as.matrix(reconstruction))^2, na.rm = TRUE)

  # Create result object
  result <- list(
    encoding = encoding,
    reconstruction = reconstruction,
    model = model,
    reconstruction_error = mse,
    params = list(
      encoding_dim = encoding_dim,
      hidden_layers = hidden_layers,
      activation = activation,
      epochs = epochs,
      learning_rate = learning_rate
    ),
    call = match.call()
  )

  class(result) <- c("tidy_autoencoder", "list")
  return(result)
}


#' Plot Autoencoder Results
#'
#' Creates visualizations of autoencoder encoding
#'
#' @param x A tidy_autoencoder object
#' @param color Optional vector for coloring points
#' @param size Point size. Default is 3
#' @param alpha Point transparency. Default is 0.7
#' @param ... Additional arguments passed to ggplot2::geom_point()
#'
#' @return A ggplot2 object
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal
plot.tidy_autoencoder <- function(x, color = NULL, size = 3, alpha = 0.7, ...) {

  if (!inherits(x, "tidy_autoencoder")) {
    stop("Object must be of class 'tidy_autoencoder'")
  }

  if (ncol(x$encoding) < 2) {
    stop("Encoding dimension must be at least 2 for visualization")
  }

  plot_data <- x$encoding

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
      title = "Autoencoder Encoding",
      subtitle = sprintf("Reconstruction Error (MSE) = %.4f", x$reconstruction_error),
      x = "Dimension 1",
      y = "Dimension 2"
    ) +
    theme_minimal()

  return(p)
}


#' Print method for tidy_autoencoder
#'
#' @param x A tidy_autoencoder object
#' @param ... Additional arguments (currently unused)
#'
#' @export
print.tidy_autoencoder <- function(x, ...) {
  cat("Autoencoder for Dimensionality Reduction (H2O)\n")
  cat("===============================================\n\n")
  cat(sprintf("Encoding dimension: %d\n", x$params$encoding_dim))
  cat(sprintf("Number of observations: %d\n", nrow(x$encoding)))
  cat(sprintf("Hidden layers: %s\n", paste(x$params$hidden_layers, collapse = ", ")))
  cat(sprintf("Activation: %s\n", x$params$activation))
  cat(sprintf("Epochs: %d\n", x$params$epochs))
  cat(sprintf("Reconstruction error (MSE): %.6f\n", x$reconstruction_error))
  cat("\nUse plot() to visualize the encoding\n")
  invisible(x)
}


#' Summary method for tidy_autoencoder
#'
#' @param object A tidy_autoencoder object
#' @param ... Additional arguments (currently unused)
#'
#' @export
summary.tidy_autoencoder <- function(object, ...) {
  cat("Autoencoder Summary (H2O)\n")
  cat("=========================\n\n")
  cat(sprintf("Encoding dimension: %d\n", object$params$encoding_dim))
  cat(sprintf("Observations: %d\n", nrow(object$encoding)))
  cat(sprintf("Input features: %d\n", ncol(object$reconstruction)))

  cat(sprintf("\nArchitecture:\n"))
  cat(sprintf("  Input: %d\n", ncol(object$reconstruction)))
  for (i in seq_along(object$params$hidden_layers)) {
    cat(sprintf("  Hidden %d: %d (%s)\n", i, object$params$hidden_layers[i], object$params$activation))
  }
  cat(sprintf("  Encoding: %d (linear)\n", object$params$encoding_dim))
  for (i in rev(seq_along(object$params$hidden_layers))) {
    cat(sprintf("  Hidden %d: %d (%s)\n", length(object$params$hidden_layers) + i,
                object$params$hidden_layers[i], object$params$activation))
  }
  cat(sprintf("  Output: %d (linear)\n", ncol(object$reconstruction)))

  cat(sprintf("\nTraining:\n"))
  cat(sprintf("  Epochs: %d\n", object$params$epochs))
  cat(sprintf("  Learning rate: %.4f\n", object$params$learning_rate))

  cat(sprintf("\nReconstruction error (MSE): %.6f\n", object$reconstruction_error))

  invisible(object)
}
