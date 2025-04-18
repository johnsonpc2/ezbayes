
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ezbayes

<!-- badges: start -->
<!-- badges: end -->

ezbayes is a wrapper package that bundles several of the basic steps when conducting Bayesian analysis in R, such as model fitting, parameter estimation, and visualizations. The package makes use of functions from the BRMS package, and is largely based on examples and code from the second edition of Doing Bayesian Data Analysis by John Kruschke.

## Installation

You can install the development version of ezbayes from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("johnsonpc2/ezbayes")
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
