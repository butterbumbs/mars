# MARS
Implementation of Multivariate Adaptive Regression Splines in R
================

## Overview & Description

This MARS package uses Freidman’s (1991) algorithms to implement
Multivariate Adaptive Regression Splines in regression models.The
benefit to using MARS is its flexibility on adapting to non-linear
regression models by using stepwise recursive partitioning on basis
functions and the interaction with predictors.

The goal of mars is to …

## Files:

| File             | Description                                                                                                                                                                                                      |
|------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `mars.R`         | Contains: mars(), with fwd_stepwise(), bwd_stepwise(), LOF(), h(), init_B(), split_points(). Also , Validator - validate_mars.control(), helper - new_mars.control and constructor for mars.control(). These………. |
| `predict.mars.R` | Contains: predict.mars(), make_B() and h(), to……….                                                                                                                                                               |
| `plot.mars.R`    | Contains: plot.mars() which generates plots for fitted vs actual, residuals vs fitted, a Q-Q plot, and residuals vs index…………                                                                                    |
| `print.mars.R`   | Contains: print.mars(), to display mars model……….                                                                                                                                                                |
| `summary.mars.R` | Contains: summary.mars(), which uses summary() method for fitted model………                                                                                                                                        |
| `test.R`         | Contains:………..                                                                                                                                                                                                   |

## Dependencies

- Base R (May be up to date version)

## Installation

You can install the development version of mars like so:

``` r
# From github repository:
devtools::install_github("butterbumbs/MARS")

# From downloaded source files:
source("mars.R")
source("predict.mars.R")
source("plot.mars.R")
source("print.mars.R")
source("summary.mars.R")
```

## Use

This is a basic example which shows you how to solve a common problem:

######################################################## 

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" alt="" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
\########################################################

## Authors

Zvikomborero Kennedy Jokonya \| <zkj@sfu.ca> \| @butterbumbs \|

## License

This project is licensed under the MIT License - see the LICENSE.md file
for details

## References

Jerome H. Friedman “Multivariate Adaptive Regression Splines,” The
Annals of Statistics, Ann. Statist. 19(1), 1-67, (March, 1991)
