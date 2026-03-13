# Plot hierarchical moving-window statistical analysis for time series.

Plots hierarchical moving-window statistical analysis for coupled time
series. It returns analysis on moving correlations using Pearson's
correlation coefficient and Mann-Kendall's correlation coefficient. The
input is a `data.frame` created by TH_coupled() function.

## Usage

``` r
TH_plotc(results, scale = NULL, output_file = NULL, mask = FALSE, mode = NULL)
```

## Arguments

- \`results\`:

  is a `data.frame` created by to TH_coupled().

- \`scale\`:

  a `numeric`, scaling factor for point sizes, autoset by default.

- \`output_file\`:

  is a `string`, optional, filename to save plot (NULL to skip saving).

- \`mask\`:

  a `logical` value. If TRUE, shows significance in main plots and hides
  p-value plots for modes without explicit p-value requests. Only
  applicable for modes: "all", "kendall_with_p", and "pearson_with_p".
  For other modes, a warning is issued and mask is ignored.

- \`mode\`:

  a `character` string specifying plots to show: "all", "pearson",
  "kendall", "both", "pearson_with_p", "kendall_with_p".

## Value

A `gridExtra` arranged plot object.

## Author

Vladimiro Andrea Boselli, (2025) <boselli.v@irea.cnr.it>

## Examples

``` r
 if (FALSE) { # \dontrun{
results <- TH_coupled(series1, series2, 4, 6)
TH_plotc(results, mask = TRUE, mode = "both")
TH_plotc(results, mode = "pearson_with_p")
} # }
```
