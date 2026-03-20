---
title: 'TimeHiVE: Hierarchical Moving-Window Time Series Analysis Toolkit in R'
authors:
  - name: Vladimiro Andrea Boselli
    orcid: 0000-0003-2985-6358
    affiliation: 1
  - name: Paolo Tagliolato
    orcid: 0000-0002-0261-313X
    affiliation: 1
  - name: Alessandro Oggioni
    orcid: 0000-0002-7997-219X
    affiliation: 1
affiliations:
  - name: Institute for Electromagnetic Sensing of the Environment of the National Research Council of Italy (IREA-CNR), Italy
    index: 1
keywords:
  - R
  - time series
  - moving-window statistics
  - Mann-Kendall
  - correlation
abstract: |
  TimeHiVE is an R package designed for hierarchical moving-window statistical analysis of time series data. The package addresses a fundamental challenge in time series analysis: the selection of appropriate window sizes for moving-window calculations. Traditional moving-window approaches require users to specify a fixed window size, which can obscure important patterns if chosen inappropriately. TimeHiVE eliminates this constraint by systematically computing statistics across all possible window sizes, enabling researchers to explore the full spectrum of temporal patterns in their data.
bibliography: paper.bib
---
# Summary

TimeHiVE by @Bos2025 is an R package designed for hierarchical moving-window statistical analysis of time series data. The package addresses a fundamental challenge in time series analysis: the selection of appropriate window sizes for moving-window calculations. Traditional moving-window approaches require users to specify a fixed window size, which can obscure important patterns if chosen inappropriately. TimeHiVE eliminates this constraint by systematically computing statistics across all possible window sizes, enabling researchers to explore the full spectrum of temporal patterns in their data.

The package provides implementations for both single time series analysis (including means, trends, and custom statistics) and coupled time series analysis (including Pearson and Mann-Kendall correlations). TimeHiVE features Mann-Kendall statistics algorithms with O(n log n) time complexity, parallel computation capabilities for unix-based systems, and customisable visualization tools. These features make it particularly valuable for environmental and climate research, where understanding phenomena across multiple timescales is essential.


# Statement of need

Moving-window statistical analysis is a fundamental technique in time series analysis, used to study the evolution of data over time in a dynamic and localized manner. This approach involves calculating statistics on subsets of consecutive data points that progressively move along the series. The technique helps identify localized trends, seasonality, and anomalies while reducing the impact of random fluctuations, highlighting meaningful patterns.

The challenge of selecting an optimal window size is well-documented in time series literature. A window that is too large may obscure important details, while one that is too narrow may be overly sensitive to noise. This problem is particularly relevant in climate science, where researchers need to examine phenomena across multiple timescales. Previous work by Brunetti and colleagues [@Bru2006; @Bru2009] demonstrated the value of hierarchical moving-window approaches for climate data analysis, but implementation required custom code.

TimeHiVE makes this analytical approach accessible to a broader research community by providing a well-documented, efficient implementation in R. The package offers several advantages over existing solutions:

1. It eliminates the need for *a priori* window size selection
2. It provides optimized implementations of common statistical tests
3. It supports both single and coupled time series analysis
4. It offers flexible visualization capabilities
5. It allows for custom statistical functions through its extensible architecture

The package is particularly valuable for environmental researchers, climate scientists, and anyone working with temporal data where patterns may manifest across different timescales. By providing a comprehensive toolkit for hierarchical moving-window analysis, TimeHiVE enables more thorough exploratory data analysis and more robust pattern detection in time series data.

TimeHiVE implements two main analytical functions: `TH_single()` for single time series analysis and `TH_coupled()` for analyzing relationships between two time series. Additionally, the `TH_tweak()` function allows users to implement custom statistical functions, making the package extensible to specialized analytical needs.

The package includes comprehensive visualization functions (`TH_plots()`, `TH_plotc()`, and `TH_plott()`) that generate heatmap-style representations of results, with time on the x-axis and window size on the y-axis. This visualization approach, inspired by Brunetti et al. [@Bru2006; @Bru2009], enables intuitive interpretation of patterns across timescales. A recent application has been carried out on precipitation time series derived from FAIR datasets provided by the eLTER Research Infrastructure
[eLTER-RI](https://elter-ri.eu/), offering a first example of their potential to support preliminary data exploration and quality assessment within eLTER [@Bos2025b].

# Usage examples

In this section we give a visual example for a single time series and a visual example for coupled time series

![Original Time Series](figures/Fig1.png)
*Figure 1: Different Moving Average vs Original Syntetic Time Series. Color scale on y-axis is meant only to match colors in Fig. 2*

![TimeHiVE Moving Average](figures/Fig2.png)
*Figure 2: Representation of all the possible moving averages for the Original Syntetic Time Series, moving average analyses represented in Fig. 1 are highlighted with comments on the results.*

![Original Coupled Series](figures/Fig3.png)
*Figure 3: Representation of two Time Series positively correlated for short periods but negatively correlated for long periods. The series are built as: `TS1 = 40 + 2*sin(t/2) - t/20 - rand(-2/3, 2/3)` and `TS2 = 15 + 2*sin(t/2) + t/7 - rand(-2/3, 2/3)`. The sine component creates short-term positive correlation, while the `t/n` terms drive long-term negative correlation.*

![Original Time Series](figures/Fig4.png)
*Figure 4: Here we show the Moving Correlation Analysis for the coupled Time Series represented in Fig. 4, the first row shows the results for Pearson's correlation coefficient (Top Left) and relative p-values (Top Right), the second row shows the same analysis with MK's correlation coefficients (Bottom Left) and relative p-values (Bottom Right). The inversion of the correlation between short and long period of analysis is quite clear.*

# Performance and implementation

TimeHiVE is implemented in R and utilizes several optimization strategies to ensure computational efficiency. The package employs parallel computation where appropriate, leveraging the `parallel` package to distribute calculations across available CPU cores. Key algorithms for Mann-Kendall Trend Test `TH_MK_Trend()` and Kendall Rank Correlation Coefficient `TH_MK_Corr()` are optimized to run O(n log n) time complexity, making them feasible for typical time series lengths encountered in environmental research [@Kni1966; @Chr2005; @Sep2025].

The package implementation follows modern R package development standards, including comprehensive documentation, unit tests, and examples. TimeHiVE is available on GitHub under the GPL-3.0 license, encouraging community contributions and extensions.

# Acknowledgements

This work has been partially funded from the Horizon Europe eLTER EnRich project Grant Agreement No. 101131751 (DOI: 10.3030/101131751)

# References
