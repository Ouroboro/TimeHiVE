#' Perform hierarchical moving-window statistical analysis for time series.
#' @description
#' Perform hierarchical moving-window statistical analysis for coupled time
#' series. It returns analysis on moving correlations using Pearson's 
#' correlation coefficient and Mann-Kendall correlation coefficient. The results
#' are in a `data.frame` to be passed to TH_plotc() function.
#'
#' @param `series1` a `numerical vector` containing NA for missing values.
#' @param `series2` a `numerical vector` containing NA for missing values. It
#' must be of the same length of `series1`.
#' @param `m` a `numeric` positive integer, subsampling parameter. Optional
#' (default: autocalculated).
#' @param `s` a `numeric` positive integer, cutoff parameters for subseries
#' legth. Optional (default: autocalculated).
#' @param `mode` a `character` string specifying the computation mode. One of:
#' "all", "pearson", "kendall", "both", "pearson_with_p", "kendall_with_p".
#' @param `alpha` a `numeric` significance level for flagging (default: 0.1).
#' @param alternative Optional, "two.sided" (Default), "greater", "less".
#' @return A result `data.frame` to pass to TH_plotc() with attributes.
#' @export
#' @importFrom parallel mclapply detectCores
#' @importFrom stats cor.test
#' @author Vladimiro Andrea Boselli, (2025) \email{boselli.v@@irea.cnr.it}
#' @examples
#'  \dontrun{
#' output <- TH_coupled(
#'   series1 = rnorm(200),
#'   series2 = rnorm(200),
#'   m = 2,
#'   s = 6,
#'   alpha = 0.05,
#'   mode = "pearson_with_p",
#'   alternative = "less"
#' )
#' }
### function TH_coupled
TH_coupled <- function(series1, series2, m = NULL, s = NULL, alpha = 0.1,
                       mode = "all", 
                       alternative = c("two.sided", "greater", "less")){
  
  alternative <- match.arg(alternative)
  valid_modes <- c("all", "pearson", "kendall", "both", 
                   "pearson_with_p", "kendall_with_p")
  if (!mode %in% valid_modes) {
    stop("Invalid mode. Choose from: ", paste(valid_modes, collapse = ", "))
  }
  
  # Check input
  if (missing(series1) || missing(series2)) stop("Both input series are required")
  if (alpha <= 0 || alpha >= 1) stop("alpha must be between 0 and 1")
  
  # Define scales
  n <- length(series1)
  if (length(series1) != length(series2)) stop("Series must have the same length")
  if (is.null(m)) m <- if (n > 250) ceiling(n / 200) else 1
  if (is.null(s)) s <- 6 * m
  
  # Sequences calculation
  seq_x <- seq(s, n, by = m)
  seq_y <- seq(s / 2, n - s / 2, by = m)
  
  LEN_MAX <- length(seq_x) * length(seq_y)
  x_values <- numeric(LEN_MAX)
  y_values <- numeric(LEN_MAX)
  
  # Populate coordinates
  index <- 1
  for (len in seq_x) {
    for (y in seq(len / 2, n - len / 2, by = m)) {
      x_values[index] <- y
      y_values[index] <- len
      index <- index + 1
    }
  }
  
  # Function to process sub-series
  compute_correlation <- function(ind) {
    center <- x_values[ind]
    len <- y_values[ind]
    start <- max(1, ceiling(center - (len - 1) / 2))
    end <- min(n, ceiling(center + (len - 1) / 2))
    
    # Extract and clean sub-series
    sub_series1 <- series1[start:end]
    sub_series2 <- series2[start:end]

    valid_indices <- is.finite(sub_series1) & complete.cases(sub_series1) & 
      is.finite(sub_series2) & complete.cases(sub_series2)
    sub_series1 <- sub_series1[valid_indices]
    sub_series2 <- sub_series2[valid_indices]
    valid_n <- length(sub_series1)
    
    # Initialize ouputs
    pearson_est <- pearson_p <- kendall_est <- kendall_p <- NA
    pearson_flag <- kendall_flag <- 2  # Default: not significant
    
    # Pearson correlation (if needed)
    if (mode %in% c("all", "pearson", "both", "pearson_with_p")) {
      if (valid_n >= s && length(unique(sub_series1)) >= 4 && 
          length(unique(sub_series2)) >= 4) {
        pearson <- tryCatch({
          ct <- cor.test(sub_series1, sub_series2, alternative = alternative, 
                         method = "pearson")
          c(ct$estimate, ct$p.value)
        }, error = function(e) c(NA, NA))
        pearson_est <- pearson[1]
        pearson_p <- pearson[2]
        if (!is.na(pearson_p) && pearson_p <= alpha) pearson_flag <- 1
      }
    }
    
    # Kendall correlation (if needed)
    if (mode %in% c("all", "kendall", "both", "kendall_with_p")) {
      if (valid_n >= s && length(unique(sub_series1)) >= 4 && 
          length(unique(sub_series2)) >= 4) {
        kendall <- tryCatch({
          kt <- TH_MK_Corr(sub_series1, sub_series2, alternative)
          c(kt$statistic, kt$p.value)
        }, error = function(e) c(NA, NA))
        kendall_est <- kendall[1]
        kendall_p <- kendall[2]
        if (!is.na(kendall_p) && kendall_p <= alpha) kendall_flag <- 1
      }
    }
    
    c(center, len, pearson_est, pearson_p, kendall_est, kendall_p, 
      pearson_flag, kendall_flag)
  }
  
  # Parallel calculation
  results_list <- parallel::mclapply(
    1:LEN_MAX, 
    compute_correlation, 
    mc.cores = parallel::detectCores() - 1
  )
  
  # Convert to data.frame
  results <- as.data.frame(do.call(rbind, results_list))
  colnames(results) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
  
  # Add attributes
  attr(results, "m") <- m
  attr(results, "s") <- s
  attr(results, "n") <- n
  attr(results, "alpha") <- alpha
  attr(results, "mode") <- mode
  
  return(results)
}