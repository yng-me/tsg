
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsg

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tsg)](https://CRAN.R-project.org/package=tsg)
[![R-CMD-check](https://github.com/yng-me/tsg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yng-me/tsg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `tsg` package provides a set of functions for generating frequency
tables, cross-tabulations, and formatted output tables. It is designed
to work with data frames and supports various customization options.

## Installation

``` r
install.packages("tsg")
```

## Basic usage

``` r
library(tsg)

generate_frequency(person_record, sex)

generate_crosstab(person_record, marital_status, sex)

person_record |> 
  generate_crosstab(marital_status, sex) |> 
  generate_output(
    path = "crosstab.xlsx",
    format = "xlsx"
  )
```

More examples [here](https://yng-me.github.io/tsg/articles/tsg.html).
