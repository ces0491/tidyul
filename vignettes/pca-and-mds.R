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


## ----basic-pca----------------------------------------------------------------
# Use iris data (4 dimensions)
iris_data <- iris %>% select(-Species)

# Perform PCA with scaling
pca_result <- tidy_pca(iris_data, scale = TRUE)

# View structure
names(pca_result)


## ----variance-----------------------------------------------------------------
# Variance explained by each PC
variance_info <- get_pca_variance(pca_result)
variance_info

# Visualize variance explained
tidy_pca_screeplot(pca_result)


## ----loadings-----------------------------------------------------------------
# Get loadings (wide format)
loadings_wide <- get_pca_loadings(pca_result)
loadings_wide

# Visualize loadings for PC1 and PC2 (use long format)
pca_result$loadings %>%
  filter(component %in% c("PC1", "PC2")) %>%
  ggplot(aes(x = variable, y = loading, fill = variable)) +
  geom_col() +
  facet_wrap(~component) +
  coord_flip() +
  labs(title = "PCA Loadings", y = "Loading Value") +
  theme_minimal() +
  theme(legend.position = "none")


## ----scores-plot--------------------------------------------------------------
# Augment data with PC scores
iris_pca <- augment_pca(pca_result, iris_data) %>%
  mutate(species = iris$Species)

# Create simple biplot (without coloring)
tidy_pca_biplot(pca_result)

# Custom score plot
iris_pca %>%
  ggplot(aes(x = PC1, y = PC2, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.95) +
  labs(title = "PCA Scores by Species",
       x = paste0("PC1 (", round(variance_info$prop_variance[1] * 100, 1), "%)"),
       y = paste0("PC2 (", round(variance_info$prop_variance[2] * 100, 1), "%)")) +
  theme_minimal()


## ----n-components-------------------------------------------------------------
# PCA returns all components by default
pca_result <- tidy_pca(iris_data, scale = TRUE)

# You can select components after the fact
first_2_scores <- pca_result$scores %>% select(.obs_id, PC1, PC2)
dim(first_2_scores)

# Rule of thumb: Keep PCs explaining 80%+ cumulative variance
variance_info %>%
  filter(cum_variance >= 0.80) %>%
  slice(1)


## ----interpretation-----------------------------------------------------------
# Create comprehensive summary
pca_summary <- tidy_summary(pca_result)
pca_summary


## ----classical-mds------------------------------------------------------------
# Classical MDS (needs distance matrix)
iris_dist <- dist(iris_data)
mds_classical <- tidy_mds_classical(iris_dist, ndim = 2)

# View coordinates
head(mds_classical$coordinates)

# Plot (without colors for simplicity)
plot_mds(mds_classical)


## ----smacof-mds---------------------------------------------------------------
# SMACOF MDS (needs distance matrix)
mds_smacof <- tidy_mds_smacof(iris_dist, ndim = 2)

# Check stress value (lower is better)
mds_smacof$stress

# Plot
plot_mds(mds_smacof)


## ----sammon-mds---------------------------------------------------------------
# Sammon mapping (needs distance matrix)
# Add small jitter to avoid duplicate observations
set.seed(123)
iris_jittered <- iris_data + matrix(rnorm(nrow(iris_data) * ncol(iris_data), 0, 0.01),
                                     nrow = nrow(iris_data))
iris_dist_jitter <- dist(iris_jittered)
mds_sammon <- tidy_mds_sammon(iris_dist_jitter, ndim = 2)

# Plot
plot_mds(mds_sammon)


## ----kruskal-mds--------------------------------------------------------------
# Kruskal's non-metric MDS (needs distance matrix)
# Use jittered data to avoid duplicate observations
mds_kruskal <- tidy_mds_kruskal(iris_dist_jitter, ndim = 2)

# View stress
mds_kruskal$stress

# Plot
plot_mds(mds_kruskal)


## ----unified-mds--------------------------------------------------------------
# Use tidy_mds_classical with distance matrix (unified interface)
# First calculate distance
iris_dist_euc <- dist(iris_data, method = "euclidean")
mds_result <- tidy_mds_classical(iris_dist_euc, ndim = 2)

# With different distance metrics
iris_dist_man <- dist(iris_data, method = "manhattan")
mds_manhattan <- tidy_mds_classical(iris_dist_man, ndim = 2)

plot_mds(mds_manhattan)


## ----compare-methods----------------------------------------------------------
# Create comparison plots
p1 <- iris_pca %>%
  ggplot(aes(x = PC1, y = PC2, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "PCA") +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- plot_mds(mds_classical) +
  labs(title = "Classical MDS") +
  theme(legend.position = "bottom")

# Note: Use patchwork or gridExtra to combine plots
# library(patchwork)
# p1 + p2


## ----custom-distance----------------------------------------------------------
# Calculate custom distance matrix
dist_matrix <- tidy_dist(iris_data, method = "euclidean")

# Convert to matrix format for MDS
dist_mat <- dist(iris_data)

# Apply MDS to distance matrix
mds_from_dist <- tidy_mds_classical(dist_mat, ndim = 2)

plot_mds(mds_from_dist)


## ----scaling-importance-------------------------------------------------------
# Without scaling
pca_noscale <- tidy_pca(iris_data, scale = FALSE)

# With scaling
pca_scale <- tidy_pca(iris_data, scale = TRUE)

# Compare variance explained
get_pca_variance(pca_noscale)$prop_variance[1]
get_pca_variance(pca_scale)$prop_variance[1]


## ----outlier-check------------------------------------------------------------
# View extreme scores
iris_pca %>%
  arrange(desc(abs(PC1))) %>%
  head(3)


## ----context-interpretation---------------------------------------------------
# Which variables drive PC1?
pca_result$loadings %>%
  filter(component == "PC1") %>%
  arrange(desc(abs(loading)))


## ----real-example-------------------------------------------------------------
# Create example: customer data with multiple dimensions
set.seed(123)
customer_list <- create_example_data(n = 200, k = 4, p = 10)
customer_features <- customer_list$data

# Apply PCA
customer_pca <- tidy_pca(customer_features, scale = TRUE)

# How many components to keep?
variance_summary <- get_pca_variance(customer_pca)
variance_summary %>%
  filter(cum_variance >= 0.80) %>%
  slice(1)

# Visualize
tidy_pca_screeplot(customer_pca)

