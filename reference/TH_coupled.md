# Perform hierarchical moving-window statistical analysis for time series.

Perform hierarchical moving-window statistical analysis for coupled time
series. It returns analysis on moving correlations using Pearson's
correlation coefficient and Mann-Kendall correlation coefficient. The
results are in a `data.frame` to be passed to TH_plotc() function.

## Usage

``` r
TH_coupled(
  series1,
  series2,
  m = NULL,
  s = NULL,
  alpha = 0.1,
  mode = "all",
  alternative = c("two.sided", "greater", "less")
)
```

## Arguments

- series1:

  a `numerical vector` containing NA for missing values.

- series2:

  a `numerical vector` containing NA for missing values. It must be of
  the same length of `series1`.

- m:

  a `numeric` positive integer, subsampling parameter. Optional
  (default: autocalculated).

- s:

  a `numeric` positive integer, cutoff parameters for subseries legth.
  Optional (default: autocalculated).

- alpha:

  a `numeric` significance level for flagging (default: 0.1).

- mode:

  a `character` string specifying the computation mode. One of: "all",
  "pearson", "kendall", "both", "pearson_with_p", "kendall_with_p".

- alternative:

  Optional, "two.sided" (Default), "greater", "less".

## Value

A result `data.frame` to pass to TH_plotc() with attributes.

## Author

Vladimiro Andrea Boselli, (2025) <boselli.v@irea.cnr.it>

## Examples

``` r
 if (FALSE) { # \dontrun{
output <- TH_coupled(
  series1 = rnorm(200),
  series2 = rnorm(200),
  m = 2,
  s = 6,
  alpha = 0.05,
  mode = "pearson_with_p",
  alternative = "less"
)
} # }
```
