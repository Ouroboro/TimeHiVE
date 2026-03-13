# Mann-Kendall Trend Test with Knight's Algorithm

Efficient implementation of the Mann-Kendall test for trend detection
using merge sort algorithm (O(n log n) complexity).

## Usage

``` r
TH_MK_Trend(data_series)
```

## Arguments

- data_series:

  a `numerical vector` containing NA for missing values.

## Value

A list with components:

- S:

  Mann-Kendall S statistic

- Var.S:

  Variance of S

- Z:

  Z-score

- p.value:

  Two-sided p-value

## Author

Vladimiro Andrea Boselli, (2025) <boselli.v@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
data <- c(1.2, 3.4, 2.5, 4.1, 5.6)
result <- TH_MK_Trend(data)
} # }
```
