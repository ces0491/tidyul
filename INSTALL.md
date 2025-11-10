# Installation Guide for tidyul

## Quick Installation from GitHub

The easiest way to install tidyul is directly from GitHub:

```r
# Install remotes if you don't have it
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install tidyul
remotes::install_github("ces0491/tidyul")
```

## Local Installation (For Development)

If you've cloned or downloaded the package locally:

### Option 1: Using devtools (Recommended)

```r
# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Set working directory to the package folder
setwd("path/to/tidyul")

# Install package
devtools::install()
```

### Option 2: Using R CMD

From the command line (terminal or command prompt):

```bash
# Navigate to the parent directory containing tidyul/
cd path/to/parent/directory

# Build the package
R CMD build tidyul

# Install the built package
R CMD INSTALL tidyul_0.1.0.tar.gz
```

### Option 3: Using RStudio

1. Open RStudio
2. File → Open Project → Navigate to `tidyul/tidyul.Rproj`
3. In the Build pane, click "Install and Restart"

## Dependencies

The package will automatically install required dependencies:

**Core dependencies:**
- dplyr (>= 1.0.0)
- ggplot2 (>= 3.3.0)
- tibble (>= 3.0.0)
- tidyr (>= 1.0.0)
- purrr (>= 0.3.0)
- rlang (>= 0.4.0)

**Statistical packages:**
- cluster (>= 2.1.0)
- dbscan (>= 1.1.0)
- arules (>= 1.6.0)
- arulesViz (>= 1.5.0)
- MASS (>= 7.3.0)
- smacof (>= 2.1.0)
- factoextra (>= 1.0.0)

**Suggested packages:**
- knitr
- rmarkdown
- testthat (>= 3.0.0)
- mclust

If you encounter dependency issues, install them manually:

```r
install.packages(c(
  "dplyr", "ggplot2", "tibble", "tidyr", "purrr", "rlang",
  "cluster", "dbscan", "arules", "arulesViz", "MASS", "smacof",
  "factoextra", "gridExtra", "scales", "magrittr"
))
```

## Verify Installation

After installation, verify it works:

```r
library(tidyul)

# Check package version
packageVersion("tidyul")

# Run a simple example
data_example <- create_example_data(n = 100, k = 3, p = 2)
km_result <- tidy_kmeans(data_example$data, k = 3)
print(km_result)
```

## Troubleshooting

### Issue: Package 'X' not available

**Solution:** Install the missing package manually:
```r
install.packages("package_name")
```

### Issue: Namespace conflicts

**Solution:** Explicitly specify package:
```r
tidyul::tidy_kmeans(data, k = 3)
```

### Issue: "Cannot remove prior installation"

**Solution:** Remove old version first:
```r
remove.packages("tidyul")
# Then reinstall
```

### Issue: Compilation errors on Windows

**Solution:** Install Rtools from: https://cran.r-project.org/bin/windows/Rtools/

### Issue: Compilation errors on Mac

**Solution:** Install Xcode Command Line Tools:
```bash
xcode-select --install
```

## Building from Source

To build the package from source with documentation:

```r
# Install roxygen2 if needed
install.packages("roxygen2")

# Document package (generates man/ files and NAMESPACE)
devtools::document()

# Run checks
devtools::check()

# Build package
devtools::build()

# Install
devtools::install()
```

## Updating the Package

To update to the latest version:

```r
# From GitHub
remotes::install_github("ces0491/tidyul", force = TRUE)

# From local source
devtools::install()
```

## Uninstalling

To remove the package:

```r
remove.packages("tidyul")
```

## Additional Resources

- Package documentation: `help(package = "tidyul")`
- README: `https://github.com/ces0491/tidyul/blob/main/README.md`
- Issues: `https://github.com/ces0491/tidyul/issues`

---
