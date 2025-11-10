## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)


## ----setup--------------------------------------------------------------------
library(tidyul)
library(dplyr)
library(ggplot2)


## ----kmeans-example-----------------------------------------------------------
# Prepare data (use only numeric columns)
iris_data <- iris %>% select(-Species)

# Apply k-means clustering
km_result <- tidy_kmeans(iris_data, k = 3, nstart = 25)

# View cluster centers
km_result$centers

# Augment original data with cluster assignments
iris_clustered <- augment_kmeans(km_result, iris_data) %>%
  mutate(actual_species = iris$Species)

# Preview results
head(iris_clustered)


## ----plot-kmeans, fig.width=7, fig.height=5-----------------------------------
# Plot clusters using augmented data
plot_clusters(iris_clustered, x_col = "Sepal.Length", y_col = "Sepal.Width")

# Compare with actual species labels
iris_clustered %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width,
             color = factor(cluster), shape = actual_species)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(color = "Cluster", shape = "Species",
       title = "K-Means Clustering vs Actual Species") +
  theme_minimal()


## ----pca-example--------------------------------------------------------------
# Perform PCA
pca_result <- tidy_pca(iris_data, scale = TRUE)

# View variance explained
pca_result$variance

# View loadings
get_pca_loadings(pca_result)

# Augment data with PC scores
iris_pca <- augment_pca(pca_result, iris_data) %>%
  mutate(species = iris$Species)

head(iris_pca)


## ----plot-pca, fig.width=7, fig.height=5--------------------------------------
# Scree plot
tidy_pca_screeplot(pca_result)

# Biplot with species coloring
# Note: biplot expects color_by to be a column name in the scores
# For now, create custom biplot
iris_pca %>%
  ggplot(aes(x = PC1, y = PC2, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "PCA Biplot", color = "Species") +
  theme_minimal()


## ----standardize--------------------------------------------------------------
# Standardize data (mean = 0, sd = 1)
iris_std <- standardize_data(iris_data)

# Check standardization
colMeans(iris_std) %>% round(10)
apply(iris_std, 2, sd) %>% round(2)


## ----scale-range--------------------------------------------------------------
# Scale to [0, 1] range
iris_scaled <- scale_to_range(iris_data, new_min = 0, new_max = 1)

summary(iris_scaled)


## ----quick-cluster------------------------------------------------------------
# Quick clustering analysis
quick_result <- quick_cluster(iris_data, max_k = 6)

# View recommendations
quick_result$recommendation
quick_result$summary


## ----numeric-data-------------------------------------------------------------
# Create example numeric data
set.seed(123)
example_list <- create_example_data(n = 100, k = 3, p = 4)
numeric_data <- example_list$data

# Apply clustering
result <- tidy_kmeans(numeric_data, k = 3)
result_augmented <- augment_kmeans(result, numeric_data)
plot_clusters(result_augmented)


## ----mixed-data, eval=FALSE---------------------------------------------------
# # Example with mixed types
# mixed_data <- data.frame(
#   age = rnorm(50, 30, 10),
#   income = rnorm(50, 50000, 15000),
#   education = sample(c("HS", "BS", "MS", "PhD"), 50, replace = TRUE),
#   married = sample(c(TRUE, FALSE), 50, replace = TRUE)
# )
# 
# # Calculate Gower distance
# gower_dist <- tidy_gower(mixed_data)
# 
# # Use with hierarchical clustering
# hc_result <- tidy_hclust(gower_dist, distance = NULL)

