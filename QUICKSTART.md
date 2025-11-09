# Quick Start Guide for tidyul

## 5-Minute Introduction

This guide will get you started with the most common unsupervised learning tasks.

## Installation

```r
# Install from GitHub
remotes::install_github("yourusername/tidyul")

# Load the package
library(tidyul)
library(dplyr)
library(ggplot2)
```

## 1. Principal Component Analysis (PCA)

Reduce dimensions while preserving variance:

```r
# Load data
data(USArrests)

# Perform PCA
pca_result <- tidy_pca(USArrests, scale = TRUE)

# How much variance is explained?
pca_result$variance
#   component  sdev variance prop_variance cum_variance
#   PC1        1.57    2.48         0.620        0.620
#   PC2        0.995   0.990        0.247        0.867

# Create visualizations
tidy_pca_screeplot(pca_result)
tidy_pca_biplot(pca_result, label_obs = TRUE)
```

**Key insight:** First 2 PCs explain 87% of variance!

## 2. K-Means Clustering

Group similar observations:

```r
# Load iris data
data(iris)
iris_numeric <- iris %>% select(-Species)

# Find optimal k
gap_result <- tidy_gap_stat(iris_numeric, max_k = 10)
optimal_k <- gap_result$recommended_k

# Perform clustering
km_result <- tidy_kmeans(iris_numeric, k = optimal_k, nstart = 25)

# View cluster centers
km_result$centers

# Add clusters to data and plot
iris_clustered <- augment_kmeans(km_result, iris)
plot_clusters(iris_clustered, cluster_col = "cluster",
              x_col = "Sepal.Length", y_col = "Petal.Length")
```

## 3. Hierarchical Clustering

Build a hierarchy of clusters:

```r
# Perform hierarchical clustering
hc_result <- tidy_hclust(USArrests, method = "average")

# Visualize dendrogram
tidy_dendrogram(hc_result, k = 4)

# Get cluster assignments
clusters <- tidy_cutree(hc_result, k = 4)

# Add to data
USArrests_clustered <- augment_hclust(hc_result, USArrests, k = 4)
```

## 4. DBSCAN (Density-Based Clustering)

Find clusters of arbitrary shape:

```r
# Suggest epsilon parameter
eps_info <- suggest_eps(iris_numeric, minPts = 5)
plot_knn_dist(eps_info$knn_distances)

# Perform DBSCAN
db_result <- tidy_dbscan(iris_numeric,
                         eps = eps_info$eps,
                         minPts = 5)

# View results (cluster 0 = noise)
db_result$summary

# Visualize with noise points in black
iris_db <- augment_dbscan(db_result, iris)
plot_clusters(iris_db, color_noise_black = TRUE)
```

## 5. PAM (K-Medoids for Mixed Data)

Cluster with mixed data types using Gower distance:

```r
# Create mixed data
car_data <- data.frame(
  mpg = mtcars$mpg,
  hp = mtcars$hp,
  transmission = factor(mtcars$am, labels = c("auto", "manual")),
  cylinders = factor(mtcars$cyl)
)

# Cluster with Gower distance
pam_result <- tidy_pam(car_data, k = 3, metric = "gower")

# View medoids (actual observations that represent clusters)
pam_result$medoids

# Check cluster quality
cat("Average silhouette width:", pam_result$silhouette_avg)
```

## 6. Market Basket Analysis

Find product associations:

```r
library(arules)
data("Groceries")

# Mine association rules
rules <- tidy_apriori(Groceries,
                      support = 0.001,
                      confidence = 0.5)

# Find top rules by lift
top_rules <- inspect_rules(rules, by = "lift", n = 10)

# What's often bought with milk?
milk_rules <- filter_rules_by_item(rules, "whole milk", where = "lhs")
inspect_rules(milk_rules, by = "lift", n = 5)

# Get recommendations
basket <- c("yogurt", "tropical fruit")
recommendations <- recommend_products(rules, basket, top_n = 5)

# Visualize
visualize_rules(rules, method = "scatter", top_n = 100)
```

## 7. Multidimensional Scaling (MDS)

Visualize distances in 2D:

```r
# Classical MDS
mds_result <- tidy_mds(eurodist, method = "classical")
plot_mds(mds_result, label_points = TRUE)

# Non-metric MDS (preserves rank order)
mds_nm <- tidy_mds(eurodist, method = "nonmetric")
plot_mds(mds_nm)
```

## Common Workflows

### Complete Clustering Analysis

```r
# 1. Prepare data
data_scaled <- standardize_data(iris_numeric, center = TRUE, scale = TRUE)

# 2. Find optimal k
optimal_k <- optimal_clusters(data_scaled, max_k = 10,
                              methods = c("silhouette", "gap"))

# 3. Compare methods
km <- tidy_kmeans(data_scaled, k = 3)
pam <- tidy_pam(data_scaled, k = 3)
hc <- tidy_hclust(data_scaled, method = "ward.D2")

# 4. Validate
sil_km <- tidy_silhouette(km$model$cluster, dist(data_scaled))
sil_pam <- tidy_silhouette(pam$model$clustering, dist(data_scaled))

# 5. Choose best method
cat("K-Means silhouette:", sil_km$avg_width, "\n")
cat("PAM silhouette:", sil_pam$avg_width, "\n")
```

### Dimensionality Reduction → Clustering

```r
# 1. Reduce dimensions with PCA
pca <- tidy_pca(USArrests, scale = TRUE)

# 2. Extract first 2 PCs
pca_data <- pca$scores %>% select(PC1, PC2)

# 3. Cluster in reduced space
km <- tidy_kmeans(pca_data, k = 4, nstart = 25)

# 4. Visualize
plot_data <- pca_data %>%
  mutate(cluster = as.factor(km$model$cluster))

ggplot(plot_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal()
```

## Tips for Success

### 1. Always Standardize
```r
# Different scales can dominate the analysis
data_std <- standardize_data(your_data, center = TRUE, scale = TRUE)
```

### 2. Use Multiple Methods
```r
# Don't rely on one method - compare!
optimal_k <- optimal_clusters(data, methods = c("silhouette", "gap", "wss"))
```

### 3. Validate Results
```r
# Always check silhouette width
sil <- tidy_silhouette(clusters, dist_matrix)
plot_silhouette(sil)
```

### 4. Visualize Everything
```r
# Plots reveal patterns numbers miss
plot_clusters(data_with_clusters)
plot_knn_dist(data)  # For DBSCAN
tidy_pca_biplot(pca_result)  # For PCA
```

### 5. Handle Mixed Data Properly
```r
# Use Gower distance for mixed types
gower_dist <- tidy_gower(mixed_data)
pam_result <- tidy_pam(mixed_data, k = 3, metric = "gower")
```

## Next Steps

- Read full documentation: `help(package = "tidyul")`
- Explore vignettes: `browseVignettes("tidyul")`
- Try with your own data!
- Check out the [README](README.md) for more examples

## Getting Help

- Documentation: `?function_name`
- Package help: `?tidyul`
- Report issues: GitHub Issues
- Ask questions: GitHub Discussions

---

Happy analyzing! 🎉
