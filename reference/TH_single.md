# Perform hierarchical moving-window statistical analysis for time series.

Perform hierarchical moving-window statistical analysis for single time
series. It returns analysis on moving average and running trends. The
results are in a `data.frame` to be passed to TH_plots() function.

## Usage

``` r
TH_single(series, m = NULL, s = NULL, mode = "all", alpha = 0.1)
```

## Arguments

- series:

  a `numerical vector` containing NA for missing values.

- m:

  a `numeric` positive integer, sub-sampling parameter. Optional
  (default: auto-calculated).

- s:

  a `numeric` positive integer, cutoff parameters for sub-series length.
  Optional (default: auto-calculated).

- mode:

  a `character` string specifying the computation mode. One of: "all"
  (default), "avg_only", "trend_only", "avg_trend", "trend_with_test",
  "avg_with_test".

- alpha:

  a `numeric` significance level for t-test flag (default: 0.1).

## Value

A result `data.frame` to pass to TH_plots(), `s`, `m` and the length of
the series are passed as silent attributes.

## Author

Vladimiro Andrea Boselli, (2025) <boselli.v@irea.cnr.it>

## Examples

``` r
 if (FALSE) { # \dontrun{
output <- TH_single(
  series = rnorm(200),
  m = 2,
  s = 6,
  mode = "avg_with_test",
  alpha = 0.05
)
} # }
```
