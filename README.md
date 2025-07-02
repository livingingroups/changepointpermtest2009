# cpt: Change Point Detection for Animal Movement Data

## TODO
- [ ]

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
prob_values <- change_point_test_pvalue(cpttestdata, q_max = 3, N = 500)
```

### Parameters

- `alpha`: Significance level for detecting change points (default: 0.05)
- `q`: Minimum segment length between potential change points (default: 4)
- `N`: Number of random permutations for the Monte Carlo test (default: 1000)
- `tol`: Maximum distance between indistinguishable positions (default: 0)

## Documentation

For more detailed information, see the package documentation and vignette:

```r
?change_point_test
```

## Citation

```
"How did they get here from there? Detecting changes of direction in terrestrial ranging" by R.W. Byrne, R.G. Noser, L.A. Bates & P.E. Jupp (2009) Animal Behaviour 77, 619-631.
```

## License

TODO: check LICENSE
This package is licensed under the GPL (>= 2) license.
