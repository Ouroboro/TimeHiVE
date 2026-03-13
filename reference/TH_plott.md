# Plot hierarchical moving-window statistical analysis for time series.

Plots hierarchical moving-window statistical analysis for custom
functions. The input is a `data.frame` created by TH_tweak() function.

## Usage

``` r
TH_plott(
  results,
  scale = NULL,
  output_file = NULL,
  colorscales = NULL,
  colorlimits = NULL
)
```

## Arguments

- \`results\`:

  is a `data.frame` created by TH_tweak().

- \`scale\`:

  a `numeric`, scaling factor for point sizes, autoset by default.

- \`output_file\`:

  is a `string`, optional, filename to save plot (NULL to skip saving).

- \`colorscales\`:

  a `list` of color scales for each function (optional). Can mix
  predefined names ("avg", "trend", "pval") and custom color vectors.

- \`colorlimits\`:

  a `list` of numeric vectors specifying color limits for each function
  (optional). Each element should be a vector of length 2 (c(min, max)).
  If NULL, limits are calculated automatically.

## Value

A `gridExtra` arranged plot object.

## Author

Vladimiro Andrea Boselli, (2025) <boselli.v@irea.cnr.it>

## Examples

``` r
 if (FALSE) { # \dontrun{
results <- TH_tweak(fun1, fun2, fun3, series1, series2)
TH_plott(results)
TH_plott(results, colorscales = list(c("blue", "white", "red"), "avg", "trend"))
} # }
```
