# Plot hierarchical moving-window statistical analysis for time series.

Plots hierarchical moving-window statistical analysis for single time
series. It returns analysis on moving average and running trends. The
input is a `data.frame` created by TH_single() function.

## Usage

``` r
TH_plots(results, scale = NULL, output_file = NULL, mask = FALSE, mode = NULL)
```

## Arguments

- results:

  is a `data.frame` created by to TH_single().

- scale:

  a `numeric`, scaling factor for point sizes, autoset by default.

- output_file:

  is a `string`, optional, filename to save plot (NULL to skip saving),
  NULL by default.

- mask:

  a `logical` value. If TRUE, enhances main plots by using point shapes
  to indicate statistical significance (if available). Only applicable
  for modes: "all", "trend_with_test", and "avg_with_test". For other
  modes, a warning is issued and mask is ignored.

- mode:

  a `character` string specifying the computation mode. One of: "all",
  "avg_only", "trend_only", "avg_trend", "trend_with_test",
  "avg_with_test". By default it takes the mode passed as attribute by
  `results`.

## Value

A `ggplot` and/or `gridExtra` arranged plot object.

## Author

Vladimiro Andrea Boselli, (2025) <boselli.v@irea.cnr.it>

## Examples

``` r
 if (FALSE) { # \dontrun{
results <- TH_single(series, 4, 6)
TH_plots(results)
TH_plots(results, output_file = output_file, mask = TRUE, mode = "avg_with_test")
} # }
```
