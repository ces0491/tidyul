#' Plot Clusters in 2D Space
#'
#' Visualize clustering results using first two dimensions or specified dimensions
#'
#' @param data A data frame with cluster assignments
#' @param cluster_col Name of cluster column (default: "cluster")
#' @param x_col X-axis variable (if NULL, uses first numeric column)
#' @param y_col Y-axis variable (if NULL, uses second numeric column)
#' @param centers Optional data frame of cluster centers
#' @param title Plot title
#' @param color_noise_black If TRUE, color noise points (cluster 0) as black (default: TRUE)
#'
#' @return A ggplot object
#' @importFrom rlang :=
#' @export
plot_clusters <- function(data, cluster_col = "cluster", x_col = NULL, y_col = NULL,
                          centers = NULL, title = "Cluster Plot", color_noise_black = TRUE) {

  # Find numeric columns if not specified
  numeric_cols <- names(data)[sapply(data, is.numeric)]

  if (is.null(x_col)) {
    x_col <- numeric_cols[1]
  }

  if (is.null(y_col)) {
    y_col <- if (length(numeric_cols) > 1) numeric_cols[2] else numeric_cols[1]
  }

  # Ensure cluster column is factor
  plot_data <- data %>%
    dplyr::mutate(!!cluster_col := as.factor(!!rlang::sym(cluster_col)))

  # Create base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = !!rlang::sym(x_col),
                                                y = !!rlang::sym(y_col),
                                                color = !!rlang::sym(cluster_col))) +
    ggplot2::geom_point(size = 2.5, alpha = 0.7) +
    ggplot2::labs(
      title = title,
      x = x_col,
      y = y_col,
      color = "Cluster"
    ) +
    ggplot2::theme_minimal()

  # Color noise points black if requested
  if (color_noise_black && "0" %in% unique(plot_data[[cluster_col]])) {
    p <- p + ggplot2::scale_color_manual(
      values = c("0" = "black", setNames(scales::hue_pal()(length(unique(plot_data[[cluster_col]])) - 1),
                                         setdiff(unique(plot_data[[cluster_col]]), "0")))
    )
  }

  # Add centers if provided
  if (!is.null(centers)) {
    p <- p + ggplot2::geom_point(
      data = centers,
      ggplot2::aes(x = !!rlang::sym(x_col), y = !!rlang::sym(y_col)),
      color = "black", size = 5, shape = 4, stroke = 2,
      inherit.aes = FALSE
    )
  }

  p
}


#' Create Elbow Plot for K-Means
#'
#' Plot total within-cluster sum of squares vs number of clusters
#'
#' @param wss_data A tibble with columns k and tot_withinss (from calc_wss)
#' @param add_line Add vertical line at suggested optimal k? (default: FALSE)
#' @param suggested_k If add_line=TRUE, which k to highlight
#'
#' @return A ggplot object
#' @export
plot_elbow <- function(wss_data, add_line = FALSE, suggested_k = NULL) {

  p <- ggplot2::ggplot(wss_data, ggplot2::aes(x = k, y = tot_withinss)) +
    ggplot2::geom_line(color = "steelblue", size = 1) +
    ggplot2::geom_point(color = "steelblue", size = 3) +
    ggplot2::labs(
      title = "Elbow Method - Total Within-Cluster Sum of Squares",
      subtitle = "Look for 'elbow' in the curve",
      x = "Number of Clusters (k)",
      y = "Total Within-Cluster SS"
    ) +
    ggplot2::theme_minimal()

  if (add_line && !is.null(suggested_k)) {
    p <- p +
      ggplot2::geom_vline(xintercept = suggested_k, linetype = "dashed", color = "red") +
      ggplot2::annotate("text", x = suggested_k, y = max(wss_data$tot_withinss) * 0.9,
                       label = sprintf("k = %d", suggested_k), color = "red", hjust = -0.2)
  }

  p
}


#' Create Cluster Comparison Plot
#'
#' Compare multiple clustering results side-by-side
#'
#' @param data Data frame with multiple cluster columns
#' @param cluster_cols Vector of cluster column names
#' @param x_col X-axis variable
#' @param y_col Y-axis variable
#'
#' @return A grid of ggplot objects
#' @export
plot_cluster_comparison <- function(data, cluster_cols, x_col, y_col) {

  plots <- purrr::map(cluster_cols, function(col) {
    plot_clusters(data, cluster_col = col, x_col = x_col, y_col = y_col,
                  title = paste("Clusters:", col))
  })

  # Combine plots
  gridExtra::grid.arrange(grobs = plots, ncol = ceiling(sqrt(length(plots))))
}


#' Plot Cluster Size Distribution
#'
#' Create bar plot of cluster sizes
#'
#' @param clusters Vector of cluster assignments
#' @param title Plot title (default: "Cluster Size Distribution")
#'
#' @return A ggplot object
#' @export
plot_cluster_sizes <- function(clusters, title = "Cluster Size Distribution") {

  cluster_counts <- tibble::tibble(cluster = as.factor(clusters)) %>%
    dplyr::count(cluster)

  ggplot2::ggplot(cluster_counts, ggplot2::aes(x = cluster, y = n, fill = cluster)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.5) +
    ggplot2::labs(
      title = title,
      x = "Cluster",
      y = "Number of Observations"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}


#' Plot Variance Explained (PCA)
#'
#' Create combined scree plot showing individual and cumulative variance
#'
#' @param variance_tbl Variance tibble from tidy_pca
#' @param threshold Horizontal line for variance threshold (default: 0.8 for 80%)
#'
#' @return A ggplot object
#' @export
plot_variance_explained <- function(variance_tbl, threshold = 0.8) {

  # Prepare data for plotting
  plot_data <- variance_tbl %>%
    dplyr::mutate(pc_num = seq_len(dplyr::n()))

  # Create dual-axis plot
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = pc_num)) +
    ggplot2::geom_col(ggplot2::aes(y = prop_variance), fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = cum_variance), color = "red", size = 1) +
    ggplot2::geom_point(ggplot2::aes(y = cum_variance), color = "red", size = 2) +
    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", color = "darkgreen") +
    ggplot2::labs(
      title = "Variance Explained by Principal Components",
      subtitle = sprintf("Red line: cumulative variance | Green line: %.0f%% threshold", threshold * 100),
      x = "Principal Component",
      y = "Proportion of Variance Explained"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_minimal()

  p1
}


#' Plot Dendrogram with Cluster Highlights
#'
#' Enhanced dendrogram with colored cluster rectangles
#'
#' @param hclust_obj Hierarchical clustering object (hclust or tidy_hclust)
#' @param k Number of clusters to highlight
#' @param title Plot title
#'
#' @return Invisibly returns hclust object (plots as side effect)
#' @export
plot_dendrogram <- function(hclust_obj, k = NULL, title = "Hierarchical Clustering Dendrogram") {

  if (inherits(hclust_obj, "tidy_hclust")) {
    hc <- hclust_obj$model
  } else {
    hc <- hclust_obj
  }

  plot(hc, main = title, xlab = "", ylab = "Height", sub = "", cex = 0.7)

  if (!is.null(k)) {
    stats::rect.hclust(hc, k = k, border = 2:(k + 1))
  }

  invisible(hc)
}


#' Create Summary Dashboard
#'
#' Generate a multi-panel summary of clustering results
#'
#' @param data Data frame with cluster assignments
#' @param cluster_col Cluster column name
#' @param validation_metrics Optional tibble of validation metrics
#'
#' @return Combined plot grid
#' @export
create_cluster_dashboard <- function(data, cluster_col = "cluster", validation_metrics = NULL) {

  plots <- list()

  # 1. Cluster scatter plot (first two numeric columns)
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_cols) >= 2) {
    plots[[1]] <- plot_clusters(data, cluster_col = cluster_col,
                                x_col = numeric_cols[1], y_col = numeric_cols[2])
  }

  # 2. Cluster sizes
  plots[[2]] <- plot_cluster_sizes(data[[cluster_col]])

  # 3. If validation metrics provided, create metrics plot
  if (!is.null(validation_metrics)) {
    # Create a text plot with metrics
    metrics_text <- sprintf(
      "Validation Metrics\n\nNumber of Clusters: %d\nAvg Silhouette: %.3f\nMin Size: %d\nMax Size: %d",
      validation_metrics$k,
      validation_metrics$avg_silhouette %||% NA,
      validation_metrics$min_size,
      validation_metrics$max_size
    )

    plots[[3]] <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = metrics_text, size = 5) +
      ggplot2::theme_void()
  }

  # Combine plots
  if (length(plots) > 0) {
    gridExtra::grid.arrange(grobs = plots, ncol = 2)
  }

  invisible(plots)
}


#' Create Distance Heatmap
#'
#' Visualize distance matrix as heatmap
#'
#' @param dist_mat Distance matrix (dist object)
#' @param cluster_order Optional vector to reorder observations by cluster
#' @param title Plot title
#'
#' @return A ggplot object
#' @export
plot_distance_heatmap <- function(dist_mat, cluster_order = NULL, title = "Distance Heatmap") {

  # Convert to matrix
  dist_matrix <- as.matrix(dist_mat)

  # Reorder if cluster order provided
  if (!is.null(cluster_order)) {
    order_idx <- order(cluster_order)
    dist_matrix <- dist_matrix[order_idx, order_idx]
  }

  # Convert to long format
  dist_long <- dist_matrix %>%
    tibble::as_tibble(rownames = "id1") %>%
    tidyr::pivot_longer(-id1, names_to = "id2", values_to = "distance")

  # Create heatmap
  ggplot2::ggplot(dist_long, ggplot2::aes(x = id1, y = id2, fill = distance)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "white", high = "red") +
    ggplot2::labs(title = title, x = "", y = "", fill = "Distance") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 6),
      axis.text.y = ggplot2::element_text(size = 6)
    )
}


#' Plot MDS Results
#'
#' Visualize MDS dimensions with optional cluster coloring
#'
#' @param mds_obj A tidy_mds object or matrix of MDS coordinates
#' @param data Original data frame (optional, for adding cluster info)
#' @param cluster_col Name of cluster column if present in data
#' @param dim1 First dimension to plot (default: 1)
#' @param dim2 Second dimension to plot (default: 2)
#' @param title Plot title
#' @param show_stress Show stress value in subtitle? (default: TRUE)
#'
#' @return A ggplot object
#' @export
plot_mds <- function(mds_obj, data = NULL, cluster_col = NULL, dim1 = 1, dim2 = 2,
                     title = "MDS Plot", show_stress = TRUE) {

  # Extract coordinates
  if (inherits(mds_obj, "tidy_mds")) {
    coords <- mds_obj$config
    stress <- mds_obj$stress
  } else if (is.matrix(mds_obj) || is.data.frame(mds_obj)) {
    coords <- as.data.frame(mds_obj)
    stress <- NULL
  } else {
    stop("mds_obj must be a tidy_mds object, matrix, or data frame")
  }

  # Create plot data - handle different coordinate formats
  if (".id" %in% names(coords)) {
    # Has .id column, select dimension columns by name
    dim_cols <- grep("^Dim", names(coords), value = TRUE)
    plot_data <- tibble::tibble(
      x = coords[[dim_cols[dim1]]],
      y = coords[[dim_cols[dim2]]]
    )
  } else {
    # No .id column, select by position
    plot_data <- tibble::tibble(
      x = coords[[dim1]],
      y = coords[[dim2]]
    )
  }

  # Add cluster info if provided
  if (!is.null(data) && !is.null(cluster_col) && cluster_col %in% names(data)) {
    plot_data$cluster <- as.factor(data[[cluster_col]])
  }

  # Create subtitle with stress
  subtitle <- NULL
  if (show_stress && !is.null(stress)) {
    subtitle <- sprintf("Stress: %.4f", stress)
  }

  # Create plot
  if ("cluster" %in% names(plot_data)) {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y, color = .data$cluster)) +
      ggplot2::geom_point(size = 2.5, alpha = 0.7) +
      ggplot2::labs(color = "Cluster")
  } else {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_point(size = 2.5, alpha = 0.7, color = "steelblue")
  }

  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = sprintf("MDS Dimension %d", dim1),
      y = sprintf("MDS Dimension %d", dim2)
    ) +
    ggplot2::theme_minimal()

  p
}


#' Plot DBSCAN Results
#'
#' Convenient wrapper to plot DBSCAN clustering results
#'
#' @param dbscan_obj A tidy_dbscan object
#' @param data Original data frame
#' @param x_col X-axis variable (if NULL, uses first numeric column)
#' @param y_col Y-axis variable (if NULL, uses second numeric column)
#' @param title Plot title
#' @param show_stats Show statistics in subtitle? (default: TRUE)
#'
#' @return A ggplot object
#' @export
plot_dbscan <- function(dbscan_obj, data, x_col = NULL, y_col = NULL,
                        title = "DBSCAN Clustering", show_stats = TRUE) {

  if (!inherits(dbscan_obj, "tidy_dbscan")) {
    stop("dbscan_obj must be a tidy_dbscan object")
  }

  # Augment data
  data_clustered <- augment_dbscan(dbscan_obj, data)

  # Create subtitle with stats
  subtitle <- NULL
  if (show_stats) {
    subtitle <- sprintf(
      "eps = %.3f, minPts = %d | Clusters: %d, Noise: %d (%.1f%%)",
      dbscan_obj$eps,
      dbscan_obj$minPts,
      dbscan_obj$n_clusters,
      dbscan_obj$n_noise,
      (dbscan_obj$n_noise / nrow(data)) * 100
    )
  }

  # Plot
  p <- plot_clusters(data_clustered, cluster_col = "cluster",
                     x_col = x_col, y_col = y_col,
                     title = title, color_noise_black = TRUE)

  if (!is.null(subtitle)) {
    p <- p + ggplot2::labs(subtitle = subtitle)
  }

  p
}


#' Plot K-Means Results
#'
#' Convenient wrapper to plot k-means clustering results
#'
#' @param kmeans_obj A tidy_kmeans object
#' @param data Original data frame
#' @param x_col X-axis variable (if NULL, uses first numeric column)
#' @param y_col Y-axis variable (if NULL, uses second numeric column)
#' @param show_centers Show cluster centers? (default: TRUE)
#' @param title Plot title
#' @param show_stats Show statistics in subtitle? (default: TRUE)
#'
#' @return A ggplot object
#' @export
plot_kmeans <- function(kmeans_obj, data, x_col = NULL, y_col = NULL,
                        show_centers = TRUE, title = "K-Means Clustering",
                        show_stats = TRUE) {

  if (!inherits(kmeans_obj, "tidy_kmeans")) {
    stop("kmeans_obj must be a tidy_kmeans object")
  }

  # Augment data
  data_clustered <- augment_kmeans(kmeans_obj, data)

  # Create subtitle with stats
  subtitle <- NULL
  if (show_stats) {
    subtitle <- sprintf(
      "k = %d | Total WSS: %.2f | Between SS: %.2f",
      kmeans_obj$metrics$k,
      kmeans_obj$metrics$tot_withinss,
      kmeans_obj$metrics$betweenss
    )
  }

  # Prepare centers if showing
  centers_df <- NULL
  if (show_centers) {
    centers_df <- kmeans_obj$centers
  }

  # Plot
  p <- plot_clusters(data_clustered, cluster_col = "cluster",
                     x_col = x_col, y_col = y_col,
                     centers = centers_df,
                     title = title, color_noise_black = FALSE)

  if (!is.null(subtitle)) {
    p <- p + ggplot2::labs(subtitle = subtitle)
  }

  p
}


#' Plot Hierarchical Clustering Results
#'
#' Complete visualization with dendrogram and cluster plot
#'
#' @param hclust_obj A tidy_hclust object
#' @param data Original data frame
#' @param k Number of clusters to cut tree
#' @param x_col X-axis variable for cluster plot (if NULL, uses first numeric column)
#' @param y_col Y-axis variable for cluster plot (if NULL, uses second numeric column)
#' @param show_dendrogram Show dendrogram? (default: TRUE)
#' @param title Plot title
#'
#' @return A ggplot object or list of plots if show_dendrogram = TRUE
#' @export
plot_hclust_results <- function(hclust_obj, data, k, x_col = NULL, y_col = NULL,
                                 show_dendrogram = TRUE, title = "Hierarchical Clustering") {

  if (!inherits(hclust_obj, "tidy_hclust")) {
    stop("hclust_obj must be a tidy_hclust object")
  }

  # Cut tree to get clusters
  clusters <- tidy_cutree(hclust_obj, k = k)

  # Add to data
  data_clustered <- data %>%
    dplyr::mutate(cluster = as.factor(clusters$cluster))

  # Create cluster plot
  subtitle <- sprintf(
    "Method: %s | Distance: %s | k = %d",
    hclust_obj$method,
    hclust_obj$dist_method,
    k
  )

  p_clusters <- plot_clusters(data_clustered, cluster_col = "cluster",
                               x_col = x_col, y_col = y_col,
                               title = title, color_noise_black = FALSE) +
    ggplot2::labs(subtitle = subtitle)

  if (show_dendrogram) {
    # Show both plots
    list(
      dendrogram = function() plot_dendrogram(hclust_obj, k = k, title = paste(title, "- Dendrogram")),
      clusters = p_clusters
    )
  } else {
    p_clusters
  }
}
