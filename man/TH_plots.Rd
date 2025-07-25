% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TH_plots.R
\name{TH_plots}
\alias{TH_plots}
\title{Plot hierarchical moving-window statistical analysis for time series.}
\usage{
TH_plots(results, scale = NULL, output_file = NULL, mask = FALSE, mode = NULL)
}
\arguments{
\item{`result`}{is a \code{data.frame} created by to TH_single().}

\item{`scale`}{a \code{numeric}, scaling factor for point sizes, autoset by
default.}

\item{`output_file`}{is a \code{string}, optional, filename to save plot (NULL to
skip saving), NULL by default.}

\item{`mask`}{a \code{logical} value. If TRUE, enhances main plots by using point
shapes to indicate statistical significance (if available). Only applicable
for modes: "all", "trend_with_test", and "avg_with_test". For other modes,
a warning is issued and mask is ignored.}

\item{`mode`}{a \code{character} string specifying the computation mode. One of:
"all", "avg_only", "trend_only", "avg_trend", "trend_with_test", "avg_with_test".
By default it takes the mode passed as attribute by \code{results}.}
}
\value{
A \code{ggplot} or \code{gridExtra} arranged plot object.
}
\description{
Plots hierarchical moving-window statistical analysis for single time
series. It returns analysis on moving average and running trends. The
input is a \code{data.frame} created by TH_single() function.
}
\examples{
 \dontrun{
results <- TH_single(series, 4, 6)
TH_plots(results)
TH_plots(results, output_file = output_file, mask = TRUE, mode = "avg_with_test")
}
}
\author{
Vladimiro Andrea Boselli, (2025) \email{boselli.v@irea.cnr.it}
}
