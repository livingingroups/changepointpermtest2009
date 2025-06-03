# cpt: Change Point Detection for Animal Movement Data

## Overview

The `cpt` package provides tools for detecting significant change points in animal movement trajectory data. Change points represent locations where movement patterns significantly change, which can indicate behavioral transitions or responses to environmental stimuli. This package implements a permutation-based approach to identify these critical points in animal tracking data.


## Installation

```r
# Install from CRAN (once available)
# install.packages("cpt")

# Or install the development version from GitHub
# install.packages("devtools")
# devtools::install_github("livingingroups/travelpaths-devel/pkgs/cpt")
```

## Usage

### Basic Example

```r
library(cpt)
data("cpttestdata", package = "cpt")

# First detect change points
result <- change_point_test(cpttestdata, alpha = 0.05, q = 3, N = 500)
head(result)

# Then extract the change points into a summarized format
change_points <- summary(result)
change_points

# Get probability values instead of binary indicators
prob_values <- change_point_test(cpttestdata, alpha = NULL, q = 3, N = 500)
```

### Parameters

- `alpha`: Significance level for detecting change points (default: 0.05)
- `q`: Minimum segment length between potential change points (default: 4)
- `N`: Number of random permutations for the Monte Carlo test (default: 1000)
- `tol`: Maximum distance between indistinguishable positions (default: 0)

## Documentation

For more detailed information, see the package documentation:

```r
?change_point_test
```

## Citation

If you use this package in your research, please cite it as:

```
Brock, K., Hirk, R., & Schwendinger, F. (2025). cpt: Change Point Detection for Animal Movement Data. R package version 0.0.1.
```

## License

This package is licensed under the GPL (>= 2) license.
