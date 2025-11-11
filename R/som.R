#' Self-Organizing Map (SOM)
#'
#' Performs Self-Organizing Map (Kohonen network) analysis on a dataset.
#' SOMs are unsupervised neural networks that produce low-dimensional representations
#' while preserving topological properties of the input data.
#'
#' @param data A data frame or tibble containing the data to analyze
#' @param grid_dim A numeric vector of length 2 specifying grid dimensions (rows, cols).
#'   Default is c(10, 10)
#' @param rlen Number of training iterations. Default is 100
#' @param alpha Learning rate, a vector of two numbers indicating the amount of change.
#'   Default is c(0.05, 0.01)
#' @param radius Neighborhood radius. Default is 1
#' @param dist_fct Distance function. Options are "sumofsquares", "euclidean", "manhattan".
#'   Default is "euclidean"
#' @param toroidal Logical; if TRUE, the grid is toroidal (edges wrap around).
#'   Default is FALSE
#' @param ... Additional arguments passed to kohonen::som()
#'
#' @return A list containing:
#'   \item{model}{The fitted SOM model (kohonen object)}
#'   \item{mapping}{Tibble with original data and unit assignments}
#'   \item{codes}{Tibble with codebook vectors for each unit}
#'   \item{distances}{Matrix of distances between units}
#'   \item{grid}{Information about the SOM grid}
#'
#' @examples
#' \dontrun{
#' library(tidyul)
#' library(dplyr)
#'
#' # Example with iris data
#' iris_numeric <- iris %>% select(where(is.numeric))
#' som_result <- tidy_som(iris_numeric, grid_dim = c(5, 5), rlen = 200)
#'
#' # View mapping
#' head(som_result$mapping)
#'
#' # Plot SOM
#' plot(som_result)
#' }
#'
#' @export
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_cols mutate select left_join distinct rename
#' @importFrom rlang .data
#' @importFrom stats median
tidy_som <- function(data,
                     grid_dim = c(10, 10),
                     rlen = 100,
                     alpha = c(0.05, 0.01),
                     radius = 1,
                     dist_fct = "euclidean",
                     toroidal = FALSE,
                     ...) {

  # Check if kohonen package is available
  if (!requireNamespace("kohonen", quietly = TRUE)) {
    stop("Package 'kohonen' is required for SOM analysis. Please install it with: install.packages('kohonen')")
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

  # Scale the data
  data_matrix <- as.matrix(scale(numeric_data))

  # Create SOM grid
  som_grid <- kohonen::somgrid(
    xdim = grid_dim[1],
    ydim = grid_dim[2],
    topo = "hexagonal",
    toroidal = toroidal
  )

  # Train SOM
  # Note: keep.data must be TRUE to get unit.classif and distances
  som_model <- kohonen::som(
    X = data_matrix,
    grid = som_grid,
    rlen = rlen,
    alpha = alpha,
    radius = radius,
    dist.fcts = dist_fct,
    keep.data = TRUE,  # Changed from 'keep' to always TRUE
    ...
  )

  # Extract results
  unit_assignments <- som_model$unit.classif

  # Get grid coordinates (use the grid we created, as it's more reliable)
  grid_pts <- som_grid$pts

  # Create a lookup table for grid coordinates
  grid_lookup <- tibble(
    unit = seq_len(nrow(grid_pts)),
    som_row = grid_pts[, 1],
    som_col = grid_pts[, 2]
  )

  # Create mapping tibble
  mapping <- tibble::as_tibble(numeric_data) %>%
    dplyr::mutate(som_unit = unit_assignments) %>%
    dplyr::left_join(grid_lookup, by = c("som_unit" = "unit"))

  # Extract codebook vectors
  codes_matrix <- som_model$codes[[1]]
  codes <- as_tibble(codes_matrix) %>%
    mutate(
      unit = seq_len(nrow(codes_matrix)),
      row = grid_pts[, 1],
      col = grid_pts[, 2]
    )

  # Calculate distances between neighboring units
  distances <- kohonen::object.distances(som_model, type = "codes")

  # Create result object
  result <- list(
    model = som_model,
    mapping = mapping,
    codes = codes,
    distances = distances,
    grid = list(
      dim = grid_dim,
      toroidal = toroidal,
      n_units = prod(grid_dim)
    ),
    call = match.call()
  )

  class(result) <- c("tidy_som", "list")
  return(result)
}


#' Plot Self-Organizing Map
#'
#' Creates various visualizations for SOM results
#'
#' @param x A tidy_som object
#' @param type Type of plot: "codes", "counts", "quality", "mapping", or "property"
#' @param property If type is "property", which variable to plot
#' @param ... Additional arguments (currently unused)
#'
#' @return A ggplot2 object
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile geom_text geom_line scale_fill_gradient scale_fill_gradient2 labs theme_minimal
plot.tidy_som <- function(x, type = "codes", property = NULL, ...) {

  if (!inherits(x, "tidy_som")) {
    stop("Object must be of class 'tidy_som'")
  }

  if (type == "counts") {
    # Plot number of observations mapped to each unit
    counts <- table(x$mapping$som_unit)
    # Use mapping coordinates directly instead of accessing grid$pts
    unit_coords <- x$mapping %>%
      select(.data$som_unit, .data$som_row, .data$som_col) %>%
      distinct(.data$som_unit, .keep_all = TRUE)
    plot_data <- tibble(
      unit = as.numeric(names(counts)),
      count = as.numeric(counts)
    ) %>%
      left_join(unit_coords, by = c("unit" = "som_unit")) %>%
      rename(row = .data$som_row, col = .data$som_col)

    p <- ggplot(plot_data, aes(x = .data$col, y = .data$row, fill = .data$count)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = .data$count), color = "white", fontface = "bold") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(
        title = "SOM Unit Counts",
        x = "Column",
        y = "Row",
        fill = "Count"
      ) +
      theme_minimal()

  } else if (type == "quality") {
    # Plot distance to closest units (quality measure)
    distances <- kohonen::object.distances(x$model, type = "codes")
    mean_dist <- apply(distances, 1, function(d) mean(d[d > 0]))

    # Use codes coordinates which already have row/col
    plot_data <- tibble(
      unit = seq_along(mean_dist),
      quality = mean_dist,
      row = x$codes$row,
      col = x$codes$col
    )

    p <- ggplot(plot_data, aes(x = .data$col, y = .data$row, fill = .data$quality)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient2(low = "green", mid = "yellow", high = "red", midpoint = median(mean_dist)) +
      labs(
        title = "SOM Quality (Mean Distance to Neighbors)",
        x = "Column",
        y = "Row",
        fill = "Distance"
      ) +
      theme_minimal()

  } else if (type == "property" && !is.null(property)) {
    # Plot a specific property across the SOM
    if (!property %in% names(x$codes)) {
      stop(paste("Property", property, "not found in codes"))
    }

    plot_data <- x$codes %>%
      select(.data$unit, .data$row, .data$col, value = !!property)

    p <- ggplot(plot_data, aes(x = .data$col, y = .data$row, fill = .data$value)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      labs(
        title = paste("SOM Component Plane:", property),
        x = "Column",
        y = "Row",
        fill = property
      ) +
      theme_minimal()

  } else {
    # Default: plot training progress
    if (is.null(x$model$changes)) {
      stop("Training changes not available. Rerun with keep = TRUE")
    }

    plot_data <- tibble(
      iteration = seq_along(x$model$changes),
      change = x$model$changes
    )

    p <- ggplot(plot_data, aes(x = .data$iteration, y = .data$change)) +
      geom_line(color = "blue", linewidth = 1) +
      labs(
        title = "SOM Training Progress",
        x = "Iteration",
        y = "Mean Distance to Closest Unit"
      ) +
      theme_minimal()
  }

  return(p)
}


#' Print method for tidy_som
#'
#' @param x A tidy_som object
#' @param ... Additional arguments (currently unused)
#'
#' @export
print.tidy_som <- function(x, ...) {
  cat("Self-Organizing Map\n")
  cat("===================\n\n")
  cat(sprintf("Grid dimensions: %d x %d (%d units)\n",
              x$grid$dim[1], x$grid$dim[2], x$grid$n_units))
  cat(sprintf("Number of observations: %d\n", nrow(x$mapping)))
  cat(sprintf("Number of variables: %d\n", ncol(x$codes) - 3))
  cat(sprintf("Toroidal grid: %s\n", x$grid$toroidal))
  cat("\nUse plot() to visualize the SOM\n")
  invisible(x)
}
