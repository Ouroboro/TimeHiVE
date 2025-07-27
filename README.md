# TimeHiVE: Hierarchical Moving-Window Time Series Analysis Toolkit

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI: 10.5281/zenodo.15697261](https://img.shields.io/badge/doi-10.5281/zenodo.15697261-yellow.svg)](https://doi.org/10.5281/zenodo.15697261)
<!-- badges: end -->

## Overview

TimeHiVE provides efficient R functions for hierarchical moving-window analysis of coupled time series data. The toolkit implements:

- Parallel computation of Pearson and Mann-Kendall correlations
- Optimized O(n log n) implementations of statistical tests
- Customizable visualization of time-dependent relationships
- Statistical significance testing with adjustable thresholds

The software package here proposed allows to perform analyses on time series avoiding the arbitrary choice of the time window and allowing to appreciate the whole spectrum of possible results offered by this kind of statistical tools.

![Original Time Series](images/Fig1.jpg)
*Figure 1: Different Moving Average vs Original Syntetic Time Series. Color scale on y-axis is meant only to match colors in Fig. 2*

![TimeHiVE Moving Average](images/Fig2.jpg)
*Figure 2: Representation of all the possible moving averages for the Original Syntetic Time Series, moving averages analyses represented in Fig. 1 are highlighted with comments on the results.*

![Original Coupled Series](images/Fig3.jpg)
*Figure 3: Representation of two Time Series positively correlated for short periods but negatively correlated for long periods. The series are built as: `TS1 = 40 + 2*sin(t/2) - t/20 - rand(-2/3, 2/3)` and `TS2 = 15 + 2*sin(t/2) + t/7 - rand(-2/3, 2/3)`. The sine component creates short-term positive correlation, while the `t/n` terms drive long-term negative correlation.*

![Original Time Series](images/Fig4.jpg)
*Figure 4: Here we show the Moving Correlation Analysis for the coupled Time Series represented in Fig. 4, the first row shows the results for Pearson’s correlation coefficient (Top Left) and relative p-values (Top Right), the second row shows the same analysis with MK’s correlation coefficients (Bottom Left) and relative p-values (Bottom Right). The inversion of the correlation between short and long period of analysis is quite clear.*

FIRST ROW: Pearson’s correlation coefficient (Top Left) and relative p-values (Top Right).
SECOND ROW: same analysis with MK’s correlation coefficientes (Bottom Left) and relative p-values (Bottom Right). The inversion of the correlation between short and long period of analysis is quite clear.

## Installation

You can install the development version from GitHub with:

```r
# install.packages("remotes")
remotes::install_github("Ouroboro/TimeHiVE")
```

## Usage

### Basic Example

```r
library(TimeHiVE)

# Generate example data
set.seed(123)
series1 <- sin(seq(0, 4*pi, length.out = 200)) + rnorm(200, sd = 0.2)
series2 <- cos(seq(0, 4*pi, length.out = 200)) + rnorm(200, sd = 0.3)

# Perform analysis
results <- TH_coupled(
  series1 = series1,
  series2 = series2,
  m = 2,
  s = 6,
  alpha = 0.05,
  mode = "both"
)

# Visualize results
TH_plotc(results, mask = TRUE, mode = "both")
```

### Main Functions

- `TH_coupled()`: Perform hierarchical moving-window analysis
- `TH_plotc()`: Visualize analysis results
- `TH_MK_Corr()`: Mann-Kendall correlation test
- `TH_MK_Trend()`: Mann-Kendall trend test

## Documentation

For detailed documentation see:

```r
?TH_coupled
?TH_plotc
?TH_MK_Corr
?TH_MK_Trend
```

## Contributing

Contributions are welcome! Please follow these steps:

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/your-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin feature/your-feature`)
5. Open a Pull Request

## License

This package is licensed under the GNU General Public License v3.0 (GPL-3.0). See the [LICENSE](LICENSE) file for details.

## Contact

For questions or issues, please contact:

Vladimiro Andrea Boselli  
Email: boselli.v@irea.cnr.it  
GitHub: [@Ouroboro](https://github.com/Ouroboro)
