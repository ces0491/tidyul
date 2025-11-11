# Global variables to avoid R CMD check NOTES
utils::globalVariables(c(
  # dplyr/tidyr variables
  ".id", ".obs_id", ".row_id", "cluster", "component", "variable",
  "loading", "variance", "prop_variance", "cum_variance", "pc_num",
  "n", "k", "tot_withinss", "gap", "SE.sim", "avg_sil_width",
  "sil_width", "neighbor", "confidence", "support", "lift", "lhs", "rhs",
  "id1", "id2", "distance", "knn_dist", "is_core", "is_noise",
  "x_end", "y_end", ":=", "avg_silhouette", "plot_order",
  "xmin", "xmax", "ymin", "ymax",
  # market basket analysis variables
  "count", "frequency", "item",
  # SOM variables
  "som_unit", "som_row", "som_col", "unit", "row", "col", "change", "iteration", "value", "quality",
  # PCA plot variables
  "contribution", "total_contrib", "total",
  # stats/base functions
  "where", "var", "rnorm", "runif", "setNames", "head"
))
