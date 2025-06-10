#' Calculate EDA Correlation Results
#'
#' @param series1 Numerical vector containing NA for missing values.
#' @param series2 Numerical vector containing NA for missing values with the same length of series1.
#' @param m Optional, scaling factor (autocalculated).
#' @param s Optional, cutoff series length (autocalculated).
#' @param alternative Optional, "two.sided" (Default), "greater", "less".
#' @return A result DataFrame to pass to plotting_double().
#' @export
#' @importFrom parallel mclapply detectCores
#' @importFrom stats cor.test lm pnorm

eda_double <- function(series1, series2, m = NULL, s = NULL,
                       alternative = c("two.sided", "greater", "less")){

  alternative <- match.arg(alternative)
  
  # Check input
  if (missing(series1) || missing(series2)) stop("Both input series are required")
  
  # Manage input
  if (is.character(series1)) series1 <- read_ts_data(series1)
  if (is.character(series2)) series2 <- read_ts_data(series2)
  
  n <- length(series1)
  if (length(series1) != length(series2)) stop("Series must have the same length")
  if (is.null(m)) m <- if (n > 250) ceiling(n / 200) else 1
  s <- 6 * m
  
  # Define scales
  seq_x <- seq(s, n, by = m)
  seq_y <- seq(s / 2, n - s / 2, by = m)
  
  LEN_MAX <- length(seq_x) * length(seq_y)
  x_values <- numeric(LEN_MAX)
  y_values <- numeric(LEN_MAX)
  
  index <- 1
  for (len in seq_x) {
    for (y in seq(len / 2, n - len / 2, by = m)) {
      x_values[index] <- y
      y_values[index] <- len
      index <- index + 1
    }
  }
  
  # Function for calculating correlations (always returns a vector of 8 elements)
  compute_correlation <- function(ind) {
    centro <- x_values[ind]
    len <- y_values[ind]
    start <- max(1, round(centro - (len - 1) / 2))
    end <- min(n, ceiling(centro + (len - 1) / 2))
    
    sub_series1 <- series1[start:end]
    sub_series2 <- series2[start:end]
    
    # Filter invalid values
    valid_indices <- is.finite(sub_series1) & complete.cases(sub_series1) & 
      is.finite(sub_series2) & complete.cases(sub_series2)
    sub_series1 <- sub_series1[valid_indices]
    sub_series2 <- sub_series2[valid_indices]
    
    # Skip if sets are too short or constant
    if (length(sub_series1) < s || length(unique(sub_series1)) < 4 || 
        length(sub_series2) < s || length(unique(sub_series2)) < 4) {
      return(rep(NA, 8))  # Returns NA instead of NULL to avoid errors
    }
    
    # Pearson correlation
    pearson <- tryCatch({
      ct <- cor.test(sub_series1, sub_series2, alternative, method = "pearson")
      c(ct$estimate, ct$p.value)
    }, error = function(e) c(NA, NA))
    
    # Kendall correlation
    kendall <- tryCatch({
      kt <- kendall_cor_test(sub_series1, sub_series2, alternative)
      c(kt$statistic, kt$p.value)
    }, error = function(e) c(NA, NA))
    
    # Returns results with significance flags
    c(centro, len, 
      pearson[1], pearson[2], 
      kendall[1], kendall[2], 
      ifelse(!is.na(pearson[2]) && pearson[2] <= 0.1, 1, 2), 
      ifelse(!is.na(kendall[2]) && kendall[2] <= 0.1, 1, 2))
  }
  
  # Parallel calculation of correlations
  results_list <- parallel::mclapply(
    1:LEN_MAX, 
    compute_correlation, 
    mc.cores = parallel::detectCores() - 1  # Use all cores except one
  )
  
  # Convert the list to a matrix
  results <- do.call(rbind, results_list)
  
  # Convert to data.frame and assign names to columns
  results <- as.data.frame(results)
  colnames(results) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
  
  return(results)
}