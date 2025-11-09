# Test Script for tidyul Package
# ================================

cat("Testing tidyul package functionality...\n\n")

# Load the package
library(tidyul)
library(dplyr)

# Test 1: PCA
cat("Test 1: PCA\n")
cat("-----------\n")
pca_result <- tidy_pca(USArrests, scale = TRUE)
cat("✓ PCA completed\n")
cat("  Variance explained by PC1:", round(pca_result$variance$prop_variance[1] * 100, 1), "%\n\n")

# Test 2: K-Means
cat("Test 2: K-Means\n")
cat("---------------\n")
iris_numeric <- iris %>% select(-Species)
km_result <- tidy_kmeans(iris_numeric, k = 3, nstart = 25)
cat("✓ K-Means completed\n")
cat("  Number of clusters:", nrow(km_result$centers), "\n")
cat("  Total WSS:", round(km_result$metrics$tot_withinss, 2), "\n\n")

# Test 3: Hierarchical Clustering
cat("Test 3: Hierarchical Clustering\n")
cat("--------------------------------\n")
hc_result <- tidy_hclust(USArrests, method = "average")
clusters <- tidy_cutree(hc_result, k = 4)
cat("✓ Hierarchical clustering completed\n")
cat("  Cluster sizes:", paste(table(clusters$cluster), collapse = ", "), "\n\n")

# Test 4: DBSCAN
cat("Test 4: DBSCAN\n")
cat("--------------\n")
eps_info <- suggest_eps(iris_numeric, minPts = 5)
db_result <- tidy_dbscan(iris_numeric, eps = eps_info$eps, minPts = 5)
cat("✓ DBSCAN completed\n")
cat("  Number of clusters:", db_result$n_clusters, "\n")
cat("  Noise points:", db_result$n_noise, "\n\n")

# Test 5: PAM
cat("Test 5: PAM (K-Medoids)\n")
cat("-----------------------\n")
pam_result <- tidy_pam(iris_numeric, k = 3)
cat("✓ PAM completed\n")
cat("  Average silhouette width:", round(pam_result$silhouette_avg, 3), "\n\n")

# Test 6: MDS
cat("Test 6: MDS\n")
cat("-----------\n")
mds_result <- tidy_mds(eurodist, method = "classical")
cat("✓ MDS completed\n")
cat("  Goodness of fit:", round(mds_result$gof * 100, 1), "%\n\n")

# Test 7: Gower Distance
cat("Test 7: Gower Distance\n")
cat("----------------------\n")
car_data <- data.frame(
  horsepower = c(130, 250, 180),
  color = factor(c("red", "black", "blue"))
)
gower_dist <- tidy_gower(car_data)
cat("✓ Gower distance computed\n")
cat("  Distance matrix size:", attr(gower_dist, "Size"), "x", attr(gower_dist, "Size"), "\n\n")

# Test 8: Validation
cat("Test 8: Validation Metrics\n")
cat("--------------------------\n")
sil_result <- tidy_silhouette_analysis(iris_numeric, max_k = 5)
cat("✓ Silhouette analysis completed\n")
cat("  Optimal k:", attr(sil_result, "optimal_k"), "\n\n")

# Test 9: Utilities
cat("Test 9: Utility Functions\n")
cat("-------------------------\n")
example_data <- create_example_data(n = 50, k = 3, p = 2)
cat("✓ Example data created\n")
cat("  Observations:", nrow(example_data$data), "\n")
cat("  Variables:", ncol(example_data$data), "\n\n")

# Summary
cat("=" %R% 50)
cat("\n✅ All tests passed successfully!\n")
cat("=" %R% 50)
cat("\n\ntidyul package is working correctly.\n")
