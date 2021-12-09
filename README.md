
<!-- README.md is generated from README.Rmd. Please edit that file -->

# norbertr

The goal of norbertr is to provide some code to help those interested in
the technicalities to understand what a drift-diffusion model is. In
particular, this package provides code giving the bivariate probability
density of a response time and binary choice, and the marginal
probability of the binary choice. It also provides a function for
drawing samples from a drift-diffusion model.

## Installation

You can install the development version of `norbertr` like so:

``` r
devtools::install_github('mark-andrews/norbertr')
```

This package uses Stan, rstan, and C++ code.

## Example

The following code gives the probability of making a binary response of
1, rather than 0, if the relative starting point is *b* = 0.5,
inter-barrier distance is *a* = 2, and drift rate is *v* = 0.5.

``` r
library(norbertr)

dchoice(b = 0.5, a = 2, v = 0.5, choice = 1)
#> [1] 0.7310586
```
