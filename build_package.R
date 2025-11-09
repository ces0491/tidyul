# Build and Install Script for tidyul Package
# ============================================

cat("Building tidyul package...\n\n")

# Check for required packages
required_pkgs <- c("devtools", "roxygen2", "testthat")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

# Load devtools
library(devtools)

cat("\n=== Step 1: Generating documentation ===\n")
try(document(), silent = FALSE)

cat("\n=== Step 2: Checking package ===\n")
check_result <- try(check(), silent = FALSE)

cat("\n=== Step 3: Installing package ===\n")
install_result <- try(install(), silent = FALSE)

if (inherits(install_result, "try-error")) {
  cat("\n❌ Installation failed. Check errors above.\n")
} else {
  cat("\n✅ Package installed successfully!\n")
  cat("\nTest it with:\n")
  cat("  library(tidyul)\n")
  cat("  ?tidyul\n")
  cat("  example_data <- create_example_data()\n")
  cat("  km <- tidy_kmeans(example_data$data, k = 3)\n")
}
