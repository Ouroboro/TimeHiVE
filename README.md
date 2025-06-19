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