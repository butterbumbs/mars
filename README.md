MARS
================

# Implementation of Multivariate Adaptive Regression Splines in R

## Overview

This MARS package uses Freidman’s (1991) algorithms to implement
Multivariate Adaptive Regression Splines in regression models.The
benefit to using MARS is its flexibility on adapting to non-linear
regression models by using step wise recursive partitioning on basis
functions and the interaction with predictors.


## Files:

| File             | Description                                                                                                                                                                                                      |
|------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `mars.R`         | Contains: mars(), with fwd_stepwise(), bwd_stepwise(), LOF(), h(), init_B(), split_points(). Also , Validator - validate_mars.control(), helper - new_mars.control and constructor for mars.control() |
| `predict.mars.R` | Contains: predict.mars(), make_B() and h(), to predict fitted values                                                                                                                                                               |
| `plot.mars.R`    | Contains: plot.mars() which generates plots for basis functions                                                                                    |
| `print.mars.R`   | Contains: print.mars(), to print coefficients                                                                                                                                                                 |
| `summary.mars.R` | Contains: summary.mars(), which uses summary() method for a fitted model                                                                                                                                      |
| `test.R`         | Contains:examples using all methods in package                                                                                                                                                                                                                                                                             |
|  `data.R`     | Contains: A test data set for the mars package                                 |

## Dependencies

- Base R (May be up to date version)

## Installation

You can install mars like so:

``` r
# From github repository:
remotes::install_github("butterbumbs/mars")
library(mars)


# From downloaded source files:
source("mars.R")
source("predict.mars.R")
source("plot.mars.R")
source("print.mars.R")
source("summary.mars.R")
```

## Author

Zvikomborero Kennedy Jokonya \| <zkj@sfu.ca> \| @butterbumbs \|

## License

This project is licensed under the MIT License - see the LICENSE.md file
for details

## References

Jerome H. Friedman “Multivariate Adaptive Regression Splines,” The
Annals of Statistics, Ann. Statist. 19(1), 1-67, (March, 1991)
