# cpt: Change Point Detection for Animal Movement Data

## TODOs:
- [ ] tests output class
- [ ] plots
- [ ] vignette
- [ ] test parallel


## Overview

Implements a permutation-based approach for detecting directional changes in animal movement trajectory data.
Methodology published in "How did they get here from there? Detecting changes of direction in terrestrial ranging"
by R.W. Byrne, R. Noser, L.A. Bates, P.E. Jupp (2009, DOI: 10.1016/j.anbehav.2008.11.014)


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
result <- change_point_test(cpttestdata, alpha = 0.05, q = 3, n = 500)
head(result)

# Then extract the change points into a summarized format
change_points <- summary(result)
change_points

# Get probability values instead of binary indicators
prob_values <- change_point_test_pvalue(cpttestdata, q_max = 3, n = 500)
```

### Parameters

- `alpha`: Significance level for detecting change points (default: 0.05)
- `q`: Minimum segment length between potential change points (default: 4)
- `n`: Number of random permutations for the Monte Carlo test (default: 1000)
- `tol`: Maximum distance between indistinguishable positions (default: 0)

## Documentation

For more detailed information, see the package documentation and vignette:

```r
?change_point_test
```

## Citation

When using this package in a scientific publication, in addition to citing this package (info in `citation()`), it may be appropriate to cite the paper that proposed this methodology.

```
"How did they get here from there? Detecting changes of direction in terrestrial ranging" by R.W. Byrne, R.G. Noser, L.A. Bates & P.E. Jupp (2009) Animal Behaviour 77, 619-631.
```

```
@article{BYRNE2009619,
title = {How did they get here from there? Detecting changes of direction in terrestrial ranging},
journal = {Animal Behaviour},
volume = {77},
number = {3},
pages = {619-631},
year = {2009},
issn = {0003-3472},
doi = {https://doi.org/10.1016/j.anbehav.2008.11.014},
url = {https://www.sciencedirect.com/science/article/pii/S0003347208005514},
author = {R.W. Byrne and R. Noser and L.A. Bates and P.E. Jupp},
keywords = {chacma baboon, change-point, chimpanzee, cognitive map, direction, , , route choice, statistical method},
abstract = {Efficient exploitation of large-scale space is crucial to many species of animal, but the difficulties of studying how animals decide on travel routes in natural environments have hampered scientific understanding of environmental cognition. Field experiments allow researchers to define travel goals for their subjects, but practical difficulties restrict large-scale studies. In contrast, data on natural travel patterns are abundant and easy to record, but hard to interpret without circularity and subjectivity when making inferences about when and why an animal began heading to a particular location. We present a method of determining objectively the point at which an animal's travel path becomes directed at a location, for instance a distant feeding site, based on the statistical characteristics of its route. We evaluate this method and illustrate how it can be tailored to particular problems, using data that are (1) synthetic, (2) from chacma baboons, Papio ursinus, where travel is from a single sleeping site in an overlapping home range, and (3) from chimpanzees, Pan troglodytes, where sleeping sites are unlimited within a large territory. We suggest that this ‘change-point test’ might usefully become a routine first step in interpreting the decision making behind animal travel under natural conditions.}
}
```

## License

TODO: check LICENSE
This package is licensed under the GPL (>= 2) license.
