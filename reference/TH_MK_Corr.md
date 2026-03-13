# Mann-Kendall Tau Test with Knight's Algorithm

Efficient implementation of the Mann-Kendall test for correlation using
Knight's algorithm of (O(n log n) complexity). Handles ties correctly.

## Usage

``` r
TH_MK_Corr(x, y, alternative = c("two.sided", "greater", "less"))
```

## Arguments

- x:

  a `numerical vector`

- y:

  a `numerical vector`

- alternative:

  Type of alternative hypothesis ("two.sided", "greater", "less")

## Value

A `htest` object with results

- statistic:

  Kendall's tau-b statistic

- p.value:

  p-value

- alternative:

  Alternative hypothesis description

## Author

Vladimiro Andrea Boselli, (2025) <boselli.v@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
kt <- TH_MK_Corr(sub_series1, sub_series2, alternative = "less")
c(kt$statistic, kt$p.value)
} # }
```
