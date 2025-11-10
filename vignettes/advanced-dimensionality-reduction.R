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


## ----isomap-basic, eval=FALSE-------------------------------------------------
# # Prepare iris data
# iris_data <- iris %>% select(-Species)
# 
# # Apply ISOMAP (requires Rdimtools package)
# isomap_result <- tidy_isomap(iris_data, dims = 2, k = 5)
# 
# # View results
# print(isomap_result)
# 
# # Access embedding
# head(isomap_result$embedding)


## ----isomap-dims, eval=FALSE--------------------------------------------------
# # 2D embedding for visualization
# isomap_2d <- tidy_isomap(iris_data, dims = 2, k = 5)
# 
# # 3D embedding for more detail
# isomap_3d <- tidy_isomap(iris_data, dims = 3, k = 5)


## ----isomap-k, eval=FALSE-----------------------------------------------------
# # Small k: More local structure, may disconnect graph
# isomap_k3 <- tidy_isomap(iris_data, dims = 2, k = 3)
# 
# # Large k: More global structure, may lose detail
# isomap_k10 <- tidy_isomap(iris_data, dims = 2, k = 10)
# 
# # Compare stress values (lower is better)
# isomap_k3$stress
# isomap_k10$stress


## ----isomap-plot, eval=FALSE--------------------------------------------------
# # Basic plot
# plot(isomap_result)
# 
# # Plot with species colors
# plot(isomap_result, color = iris$Species)
# 
# # Custom visualization
# isomap_result$embedding %>%
#   mutate(species = iris$Species) %>%
#   ggplot(aes(x = Dim1, y = Dim2, color = species)) +
#   geom_point(size = 3, alpha = 0.7) +
#   stat_ellipse() +
#   labs(title = "ISOMAP Embedding of Iris Data",
#        subtitle = paste("Stress =", round(isomap_result$stress, 3))) +
#   theme_minimal()


## ----isomap-tune-k, eval=FALSE------------------------------------------------
# # Try different k values
# k_values <- c(3, 5, 7, 10, 15, 20)
# stress_values <- numeric(length(k_values))
# 
# for (i in seq_along(k_values)) {
#   result <- tidy_isomap(iris_data, dims = 2, k = k_values[i])
#   stress_values[i] <- result$stress
# }
# 
# # Plot stress vs k
# data.frame(k = k_values, stress = stress_values) %>%
#   ggplot(aes(x = k, y = stress)) +
#   geom_line() +
#   geom_point(size = 3) +
#   labs(title = "ISOMAP: Stress vs Number of Neighbors",
#        x = "Number of Neighbors (k)",
#        y = "Stress") +
#   theme_minimal()


## ----som-basic, eval=FALSE----------------------------------------------------
# # Apply SOM (requires kohonen package)
# som_result <- tidy_som(
#   iris_data,
#   grid_dim = c(5, 5),  # 5x5 grid
#   rlen = 100           # 100 training iterations
# )
# 
# # View results
# print(som_result)
# 
# # View mapping (which grid cell each observation maps to)
# head(som_result$mapping)


## ----som-grid, eval=FALSE-----------------------------------------------------
# # Small grid (coarse representation)
# som_small <- tidy_som(iris_data, grid_dim = c(3, 3), rlen = 100)
# 
# # Large grid (fine-grained representation)
# som_large <- tidy_som(iris_data, grid_dim = c(10, 10), rlen = 100)
# 
# # Rectangular grid
# som_rect <- tidy_som(iris_data, grid_dim = c(8, 5), rlen = 100)


## ----som-plots, eval=FALSE----------------------------------------------------
# # Unit counts (how many observations per cell)
# plot(som_result, type = "counts")
# 
# # Quality measure (distance to neighbors)
# plot(som_result, type = "quality")
# 
# # Component planes (show one variable across the SOM)
# plot(som_result, type = "property", property = "Sepal.Length")
# plot(som_result, type = "property", property = "Petal.Width")


## ----som-training, eval=FALSE-------------------------------------------------
# # More training iterations for better convergence
# som_long <- tidy_som(
#   iris_data,
#   grid_dim = c(5, 5),
#   rlen = 500,          # More iterations
#   alpha = c(0.05, 0.01) # Learning rate (start, end)
# )
# 
# # Different distance functions
# som_manhattan <- tidy_som(
#   iris_data,
#   grid_dim = c(5, 5),
#   rlen = 100,
#   dist_fct = "manhattan"
# )


## ----som-interpret, eval=FALSE------------------------------------------------
# # Map observations to SOM and color by species
# som_result$mapping %>%
#   mutate(species = iris$Species) %>%
#   ggplot(aes(x = som_col, y = som_row, color = species)) +
#   geom_point(size = 4, alpha = 0.7) +
#   labs(title = "SOM: Observation Mapping Colored by Species",
#        x = "SOM Column", y = "SOM Row") +
#   theme_minimal() +
#   theme(panel.grid = element_blank())
# 
# # View codebook vectors (prototypes for each cell)
# head(som_result$codes)


## ----autoencoder-basic, eval=FALSE--------------------------------------------
# # Apply autoencoder (requires h2o package)
# ae_result <- tidy_autoencoder(
#   iris_data,
#   encoding_dim = 2,        # Bottleneck dimension
#   hidden_layers = c(8, 4), # Hidden layer sizes
#   epochs = 50              # Training epochs
# )
# 
# # View results
# print(ae_result)
# 
# # Access encoding
# head(ae_result$encoding)
# 
# # Check reconstruction quality
# ae_result$reconstruction_error


## ----autoencoder-arch, eval=FALSE---------------------------------------------
# # Simple autoencoder (fewer layers)
# ae_simple <- tidy_autoencoder(
#   iris_data,
#   encoding_dim = 2,
#   hidden_layers = c(4),  # Single hidden layer
#   epochs = 50
# )
# 
# # Deep autoencoder (more layers)
# ae_deep <- tidy_autoencoder(
#   iris_data,
#   encoding_dim = 2,
#   hidden_layers = c(16, 8, 4), # Three hidden layers
#   epochs = 100
# )
# 
# # Compare reconstruction errors
# ae_simple$reconstruction_error
# ae_deep$reconstruction_error


## ----autoencoder-training, eval=FALSE-----------------------------------------
# # Adjust learning rate
# ae_fast <- tidy_autoencoder(
#   iris_data,
#   encoding_dim = 2,
#   hidden_layers = c(8, 4),
#   epochs = 50,
#   learning_rate = 0.01  # Faster learning
# )
# 
# # Different activation functions
# ae_tanh <- tidy_autoencoder(
#   iris_data,
#   encoding_dim = 2,
#   hidden_layers = c(8, 4),
#   activation = "Tanh",  # Use Tanh instead of Rectifier
#   epochs = 50
# )
# 
# # Set random seed for reproducibility
# ae_repro <- tidy_autoencoder(
#   iris_data,
#   encoding_dim = 2,
#   hidden_layers = c(8, 4),
#   epochs = 50,
#   seed = 123
# )


## ----autoencoder-plot, eval=FALSE---------------------------------------------
# # Basic plot
# plot(ae_result)
# 
# # Plot with colors
# plot(ae_result, color = iris$Species)
# 
# # Custom visualization
# ae_result$encoding %>%
#   mutate(species = iris$Species) %>%
#   ggplot(aes(x = Dim1, y = Dim2, color = species)) +
#   geom_point(size = 3, alpha = 0.7) +
#   stat_ellipse() +
#   labs(title = "Autoencoder Embedding",
#        subtitle = paste("Reconstruction Error =",
#                        round(ae_result$reconstruction_error, 4))) +
#   theme_minimal()


## ----autoencoder-quality, eval=FALSE------------------------------------------
# # View original vs reconstructed data
# comparison <- bind_cols(
#   original = iris_data[1:3, ],
#   reconstructed = ae_result$reconstruction[1:3, ]
# )
# comparison
# 
# # Calculate per-observation reconstruction error
# reconstruction_errors <- rowMeans((iris_data - ae_result$reconstruction)^2)
# 
# # Find observations that are hard to reconstruct
# worst_reconstructions <- order(reconstruction_errors, decreasing = TRUE)[1:5]
# iris_data[worst_reconstructions, ]


## ----compare-all, eval=FALSE--------------------------------------------------
# # Apply all methods
# isomap_res <- tidy_isomap(iris_data, dims = 2, k = 5)
# som_res <- tidy_som(iris_data, grid_dim = c(5, 5), rlen = 100)
# ae_res <- tidy_autoencoder(iris_data, encoding_dim = 2,
#                            hidden_layers = c(8, 4), epochs = 50)
# 
# # Create comparison plots
# p1 <- isomap_res$embedding %>%
#   mutate(species = iris$Species) %>%
#   ggplot(aes(x = Dim1, y = Dim2, color = species)) +
#   geom_point(size = 2, alpha = 0.7) +
#   labs(title = "ISOMAP") +
#   theme_minimal() +
#   theme(legend.position = "none")
# 
# p2 <- som_res$mapping %>%
#   mutate(species = iris$Species) %>%
#   ggplot(aes(x = som_col, y = som_row, color = species)) +
#   geom_point(size = 2, alpha = 0.7) +
#   labs(title = "SOM", x = "Column", y = "Row") +
#   theme_minimal() +
#   theme(legend.position = "none")
# 
# p3 <- ae_res$encoding %>%
#   mutate(species = iris$Species) %>%
#   ggplot(aes(x = Dim1, y = Dim2, color = species)) +
#   geom_point(size = 2, alpha = 0.7) +
#   labs(title = "Autoencoder") +
#   theme_minimal()
# 
# # Combine plots (requires gridExtra or patchwork)
# # library(gridExtra)
# # grid.arrange(p1, p2, p3, ncol = 2)


## ----scaling-tip, eval=FALSE--------------------------------------------------
# # Scale is built into functions
# isomap_result <- tidy_isomap(iris_data, dims = 2, k = 5, scale = TRUE)
# ae_result <- tidy_autoencoder(iris_data, encoding_dim = 2, scale = TRUE)


## ----tuning-tip, eval=FALSE---------------------------------------------------
# # ISOMAP: Try different k values
# # SOM: Try different grid sizes and training lengths
# # Autoencoders: Try different architectures and epochs


## ----validation-tip, eval=FALSE-----------------------------------------------
# # ISOMAP: Check stress
# isomap_result$stress
# 
# # Autoencoder: Check reconstruction error
# ae_result$reconstruction_error


## ----compute-tip, eval=FALSE--------------------------------------------------
# # ISOMAP: O(n^3) complexity - can be slow for large n
# # SOM: Fast training, scales well
# # Autoencoders: Requires h2o initialization, GPU can help


## ----real-example, eval=FALSE-------------------------------------------------
# # Create larger example dataset
# set.seed(123)
# example_list <- create_example_data(n = 500, k = 5, p = 20)
# example_data <- example_list$data
# 
# # Compare methods
# isomap_ex <- tidy_isomap(example_data, dims = 2, k = 10)
# som_ex <- tidy_som(example_data, grid_dim = c(10, 10), rlen = 200)
# ae_ex <- tidy_autoencoder(example_data, encoding_dim = 2,
#                           hidden_layers = c(16, 8), epochs = 100)
# 
# # Evaluate
# isomap_ex$stress
# ae_ex$reconstruction_error
# 
# # Visualize best method
# plot(isomap_ex, color = example_list$clusters)

