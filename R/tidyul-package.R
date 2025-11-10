#' tidyul: Tidy Unsupervised Learning
#'
#' A comprehensive toolkit for unsupervised learning analysis built on tidyverse principles.
#' Provides simplified interfaces for:
#' \itemize{
#'   \item Principal Component Analysis (PCA)
#'   \item Multidimensional Scaling (MDS) - classical, metric, non-metric, Sammon, Kruskal
#'   \item Hierarchical Clustering
#'   \item K-Means and K-Medoids (PAM/CLARA)
#'   \item DBSCAN (Density-Based Clustering)
#'   \item Distance/Dissimilarity Measures (including Gower distance)
#'   \item Cluster Validation (Silhouette, Gap Statistic)
#'   \item Market Basket Analysis (Apriori algorithm)
#'   \item Comprehensive Visualizations
#' }
#'
#' All functions follow tidy data principles with consistent interfaces and extensive documentation.
#'
#' @section Main Functions:
#'
#' **Dimensionality Reduction:**
#' \itemize{
#'   \item \code{\link{tidy_pca}}: Principal Component Analysis
#'   \item \code{\link{tidy_mds}}: Multidimensional Scaling (unified interface)
#' }
#'
#' **Clustering:**
#' \itemize{
#'   \item \code{\link{tidy_kmeans}}: K-Means clustering
#'   \item \code{\link{tidy_pam}}: PAM (Partitioning Around Medoids)
#'   \item \code{\link{tidy_clara}}: CLARA (for large datasets)
#'   \item \code{\link{tidy_hclust}}: Hierarchical clustering
#'   \item \code{\link{tidy_dbscan}}: DBSCAN density-based clustering
#' }
#'
#' **Validation:**
#' \itemize{
#'   \item \code{\link{tidy_silhouette}}: Silhouette analysis
#'   \item \code{\link{tidy_gap_stat}}: Gap statistic
#'   \item \code{\link{optimal_clusters}}: Find optimal number of clusters
#' }
#'
#' **Market Basket Analysis:**
#' \itemize{
#'   \item \code{\link{tidy_apriori}}: Mine association rules
#'   \item \code{\link{inspect_rules}}: View and filter rules
#'   \item \code{\link{recommend_products}}: Generate recommendations
#' }
#'
#' @section Getting Started:
#'
#' See the package vignettes for detailed examples:
#' \code{browseVignettes("tidyul")}
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom tibble tibble
#' @importFrom rlang .data
## usethis namespace: end
NULL
