<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

**tidyul** is an R package for unsupervised learning that follows tidyverse principles. It provides simplified, consistent interfaces for:

- 📊 **Principal Component Analysis (PCA)**
- 📐 **Multidimensional Scaling (MDS)** - Classical, SMACOF, Sammon, Kruskal
- 🧠 **Advanced Dimensionality Reduction** - ISOMAP, Autoencoders, Self-Organizing Maps
- 🌳 **Hierarchical Clustering** - All linkage methods
- 🎯 **K-Means & K-Medoids** - Including PAM and CLARA
- 🔍 **DBSCAN** - Density-based clustering with parameter tuning
- 📏 **Distance Measures** - Including Gower distance for mixed data
- ✅ **Validation Metrics** - Silhouette, Gap Statistic, and more
- 🛒 **Market Basket Analysis** - Apriori algorithm with tidy output
- 📈 **Rich Visualizations** - Built on ggplot2

All functions return tidy tibbles and work seamlessly with dplyr, ggplot2, and the tidyverse ecosystem.
Nothing fancy here, just some syntactic sugar to simplify and standardize unsupervised learning techniques.

## Installation

You can install tidyul directly from GitHub:

```r
# install.packages("remotes")
remotes::install_github("ces0491/tidyul")
```

## Quick Start

### Principal Component Analysis

```r
library(tidyul)
library(dplyr)
library(ggplot2)

# Perform PCA
pca_result <- tidy_pca(USArrests, scale = TRUE)

# View variance explained
pca_result$variance

# Create scree plot
tidy_pca_screeplot(pca_result)

# Create biplot
tidy_pca_biplot(pca_result, pc_x = 1, pc_y = 2, label_obs = TRUE)
```

### K-Means Clustering

```r
# Find optimal k
optimal_k <- optimal_clusters(iris, max_k = 10, methods = c("silhouette", "gap"))

# Perform k-means
km_result <- tidy_kmeans(iris, k = 3, nstart = 25)

# View results
km_result$clusters
km_result$centers

# Visualize
augmented_data <- augment_kmeans(km_result, iris)
plot_clusters(augmented_data, cluster_col = "cluster",
              x_col = "Sepal.Length", y_col = "Sepal.Width")
```

### Hierarchical Clustering

```r
# Perform hierarchical clustering with automatic scaling
hc_result <- tidy_hclust(USArrests, method = "average", distance = "euclidean", scale = TRUE)

# Plot dendrogram
tidy_dendrogram(hc_result, k = 4)

# Cut tree to get clusters
clusters <- tidy_cutree(hc_result, k = 4)

# Augment original data
augmented <- augment_hclust(hc_result, USArrests, k = 4)
```

### DBSCAN Clustering

```r
# Suggest epsilon parameter
eps_info <- suggest_eps(iris, minPts = 5)
plot_knn_dist(eps_info$knn_distances)

# Perform DBSCAN
db_result <- tidy_dbscan(iris, eps = eps_info$eps, minPts = 5)

# View results
db_result$clusters
db_result$summary

# Visualize
augmented <- augment_dbscan(db_result, iris)
plot_clusters(augmented, cluster_col = "cluster", color_noise_black = TRUE)
```

### Multidimensional Scaling

```r
# Classical MDS
mds_result <- tidy_mds(eurodist, method = "classical")

# Non-metric MDS
mds_result <- tidy_mds(eurodist, method = "nonmetric")

# Sammon mapping
mds_result <- tidy_mds(eurodist, method = "sammon")

# Visualize
plot_mds(mds_result, label_points = TRUE)
```

### Advanced Dimensionality Reduction

```r
# ISOMAP - preserves geodesic distances
iris_numeric <- iris %>% select(where(is.numeric))
isomap_result <- tidy_isomap(iris_numeric, dims = 2, k = 5)

# Visualize
isomap_result$embedding %>%
  mutate(species = iris$Species) %>%
  ggplot(aes(x = Dim1, y = Dim2, color = species)) +
  geom_point(size = 3)

# Self-Organizing Map (SOM)
som_result <- tidy_som(iris_numeric, grid_dim = c(5, 5), rlen = 200)
plot(som_result, type = "counts")
plot(som_result, type = "quality")

# Autoencoder (requires h2o)
ae_result <- tidy_autoencoder(
  iris_numeric,
  encoding_dim = 2,
  hidden_layers = c(8, 4),
  epochs = 50
)
plot(ae_result)
```

### Gower Distance for Mixed Data

```r
# Create data with mixed types
car_data <- data.frame(
  horsepower = c(130, 250, 180, 95, 220),
  weight = c(1200, 1650, 1420, 980, 1580),
  price_bracket = factor(c("low", "high", "mid", "low", "high")),
  color = factor(c("red", "black", "blue", "white", "silver"))
)

# Compute Gower distance
gower_dist <- tidy_gower(car_data)

# Use with PAM
pam_result <- tidy_pam(car_data, k = 2, metric = "gower")
```

### Market Basket Analysis

```r
library(arules)
data("Groceries")

# Mine association rules
rules <- tidy_apriori(Groceries, support = 0.001, confidence = 0.5)

# Inspect top rules
inspect_rules(rules, by = "lift", n = 10)

# Find related items
related <- find_related_items(rules, item = "whole milk", min_lift = 2.0)

# Visualize
visualize_rules(rules, method = "scatter", top_n = 50)

# Get recommendations
basket <- c("yogurt", "other vegetables")
recommendations <- recommend_products(rules, basket = basket, top_n = 5)
```

### Validation and Model Selection

```r
# Silhouette analysis with scaling
sil_results <- tidy_silhouette_analysis(iris, max_k = 10, scale = TRUE)
plot_silhouette(sil_results)

# Classic silhouette plot with average line
clusters <- tidy_kmeans(iris, k = 3)$cluster
dist_mat <- dist(iris[, 1:4])
sil <- cluster::silhouette(clusters, dist_mat)
plot_silhouette_classic(sil)  # Includes dashed line at average

# Gap statistic
gap_results <- tidy_gap_stat(iris, max_k = 10, B = 50)
plot_gap_stat(gap_results, show_methods = TRUE)

# Compare multiple methods
comparison <- optimal_clusters(iris, max_k = 10,
                               methods = c("silhouette", "gap", "wss"))
```

## Key Features

### Tidy Principles

All functions follow tidyverse design principles:

- **Tidy input/output**: Functions accept and return tibbles
- **Consistent interfaces**: Similar arguments across functions
- **Pipe-friendly**: Works seamlessly with `%>%` and `|>`
- **Informative**: Clear error messages and warnings

### Comprehensive Documentation

Every function includes:

- Detailed parameter descriptions
- Return value specifications
- Multiple examples
- References to methodology

### Rich Visualizations

Built-in plotting functions using ggplot2:

- Scree plots and biplots for PCA
- Dendrograms for hierarchical clustering
- Cluster scatter plots
- Silhouette and gap statistic plots
- k-NN distance plots for DBSCAN
- Association rule visualizations

### Performance

- Optimized for speed with vectorized operations
- Parallel processing support where applicable
- Efficient memory usage
- Scalable to large datasets (CLARA for k-medoids)

## Function Overview

### Dimensionality Reduction

| Function | Description |
|----------|-------------|
| `tidy_pca()` | Principal Component Analysis |
| `tidy_pca_biplot()` | PCA biplot visualization |
| `tidy_pca_screeplot()` | Scree plot of variance explained |
| `tidy_mds()` | Unified MDS interface |
| `tidy_mds_classical()` | Classical metric MDS |
| `tidy_mds_smacof()` | SMACOF algorithm (metric/non-metric) |
| `tidy_mds_sammon()` | Sammon mapping |
| `tidy_mds_kruskal()` | Kruskal's isoMDS |
| `tidy_isomap()` | ISOMAP (Isometric Feature Mapping) |
| `tidy_som()` | Self-Organizing Maps (Kohonen networks) |
| `tidy_autoencoder()` | Neural network autoencoder for dimensionality reduction |

### Clustering

| Function | Description |
|----------|-------------|
| `tidy_kmeans()` | K-Means clustering |
| `tidy_pam()` | PAM (Partitioning Around Medoids) |
| `tidy_clara()` | CLARA (for large datasets) |
| `tidy_hclust()` | Hierarchical clustering |
| `tidy_dbscan()` | DBSCAN density-based clustering |
| `tidy_cutree()` | Cut hierarchical tree |
| `suggest_eps()` | Suggest DBSCAN epsilon parameter |

### Distance/Dissimilarity

| Function | Description |
|----------|-------------|
| `tidy_gower()` | Gower distance for mixed data types |
| `tidy_dist()` | Compute distance matrices |
| `standardize_data()` | Center and scale data |

### Validation

| Function | Description |
|----------|-------------|
| `tidy_silhouette()` | Silhouette analysis |
| `tidy_gap_stat()` | Gap statistic |
| `tidy_silhouette_analysis()` | Silhouette across multiple k |
| `optimal_clusters()` | Find optimal number of clusters |
| `calc_validation_metrics()` | Comprehensive metrics |
| `compare_clusterings()` | Compare multiple results |

### Market Basket Analysis

| Function | Description |
|----------|-------------|
| `tidy_apriori()` | Mine association rules |
| `tidy_rules()` | Convert rules to tibble |
| `inspect_rules()` | View and sort rules |
| `filter_rules_by_item()` | Subset rules by item |
| `find_related_items()` | Find item associations |
| `recommend_products()` | Generate recommendations |
| `visualize_rules()` | Visualize association rules |

### Visualization

| Function | Description |
|----------|-------------|
| `plot_clusters()` | Cluster scatter plot |
| `plot_mds()` | MDS configuration plot |
| `plot_silhouette()` | Silhouette plot (multiple k) |
| `plot_silhouette_classic()` | Classic silhouette plot with average line |
| `plot_gap_stat()` | Gap statistic plot |
| `plot_knn_dist()` | k-NN distance plot (DBSCAN) |
| `plot_elbow()` | Elbow plot for k-means |
| `tidy_dendrogram()` | Hierarchical dendrogram |

### Utilities

| Function | Description |
|----------|-------------|
| `augment_pca()` | Add PC scores to data |
| `augment_kmeans()` | Add cluster assignments |
| `quick_cluster()` | Quick clustering with auto k |
| `create_example_data()` | Generate synthetic data |
| `export_clusters()` | Export results to CSV |

## Learning Resources

### Vignettes

```r
# List all vignettes (requires installation with build_vignettes = TRUE)
browseVignettes("tidyul")

# Key vignettes
vignette("introduction", package = "tidyul")
vignette("pca-and-mds", package = "tidyul")
vignette("clustering-methods", package = "tidyul")
vignette("advanced-dimensionality-reduction", package = "tidyul")
vignette("market-basket-analysis", package = "tidyul")
```

### Example Datasets

The package includes examples using standard R datasets:

- `USArrests` - PCA and hierarchical clustering
- `iris` - K-means and validation
- `mtcars` - Mixed clustering methods
- `eurodist` - MDS examples
- `Groceries` (from arules) - Market basket analysis

## Comparison with Other Packages

| Feature | tidyul | stats | cluster | factoextra |
|---------|--------|-------|---------|------------|
| Tidy output | ✅ | ❌ | ❌ | Partial |
| Unified interface | ✅ | ❌ | ❌ | Partial |
| Gower distance | ✅ | ❌ | ✅ | ❌ |
| DBSCAN | ✅ | ❌ | ❌ | ❌ |
| Market basket | ✅ | ❌ | ❌ | ❌ |
| Pipe-friendly | ✅ | ❌ | ❌ | ✅ |
| ggplot2 viz | ✅ | ❌ | ❌ | ✅ |

## License

MIT License. See [LICENSE](LICENSE) for details.

## Acknowledgments

This package builds on the work of:

- `cluster` package for PAM/CLARA
- `dbscan` package for DBSCAN
- `arules` package for association rules
- `smacof` package for MDS
- `MASS` package for classical methods
- `Rdimtools` package for ISOMAP
- `kohonen` package for Self-Organizing Maps
- `h2o` package for deep learning autoencoders
- The `tidyverse` team for design principles

## Getting Help

- 📖 Documentation: Run `?tidyul` or `help(package = "tidyul")`

---
