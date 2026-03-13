# Perform hierarchical moving-window statistical analysis for time series.

Perform hierarchical moving-window statistical analysis using custom
functions. It returns analysis results in a `data.frame` to be passed to
TH_plott() function.

## Usage

``` r
TH_tweak(..., series, m = NULL, s = NULL, param = 0.1)
```

## Arguments

- ...:

  User-defined functions for analysis

- series:

  A `list` of `numerical vectors` or a single `numerical vector`

- m:

  a `numeric` positive integer, subsampling parameter. Optional
  (default: autocalculated).

- s:

  a `numeric` positive integer, cutoff parameters for subseries length.
  Optional (default: autocalculated).

- param:

  a `numeric` significance level for flagging (default: 0.1).

## Value

A result `data.frame` to pass to TH_plott() with attributes.

## Author

Vladimiro Andrea Boselli, (2025) <boselli.v@irea.cnr.it>

## Examples

``` r
 if (FALSE) { # \dontrun{
output <- TH_tweak(
  function(x, y) cor.test(x, y, method = "pearson")$estimate,
  function(x, y) cor.test(x, y, method = "kendall")$estimate,
  series = list(serie1, serie2)
)
} # }
```
