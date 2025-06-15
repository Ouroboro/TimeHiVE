#' Perform hierarchical moving-window statistical analysis for time series.
#' @description
#' Perform hierarchical moving-window statistical analysis for single time
#' series. It returns analysis on moving average and running trends. The results
#' are in a `data.frame` to be passed to TH_plots() function.
#' 
#' @param `series` a `numerical vector` containing NA for missing values.
#' @param `m` a `numeric` positive integer, sub-sampling parameter. Optional
#' (default: auto-calculated).
#' @param `s` a `numeric` positive integer, cutoff parameters for sub-series
#' length. Optional (default: auto-calculated).
#' @param `mode` a `character` string specifying the computation mode. One of:
#' "all" (default), "avg_only", "trend_only", "avg_trend", "trend_with_test", "avg_with_test".
#' @param `alpha` a `numeric` significance level for t-test flag (default: 0.1).
#' @return A result `data.frame` to pass to TH_plots(), `s`, `m` and the length
#' of the series are passed as silent attributes.
#' @export
#' @importFrom parallel mclapply detectCores
#' @importFrom stats t.test lm pnorm
#' @author Vladimiro Andrea Boselli, (2025) \email{boselli.v@@irea.cnr.it}
#' @examples
#'  \dontrun{
#' output <- TH_single(
#'   series = rnorm(200),
#'   m = 2,
#'   s = 6,
#'   mode = "avg_with_test",
#'   alpha = 0.05
#' )
#' }
### function TH_single
TH_single <- function(series, m = NULL, s = NULL, mode = "all", alpha = 0.1) {
  
  valid_modes <- c("all", "avg_only", "trend_only", "avg_trend",
                   "trend_with_test", "avg_with_test")
  if (!mode %in% valid_modes) {
    stop("Invalid mode. Choose from: ", paste(valid_modes, collapse = ", "))
  }
  
  # Check input
  if (missing(series)) stop("Input 'series' is required")
  if (alpha <= 0 || alpha >= 1) stop("alpha must be between 0 and 1")
  
  # Define scales
  n <- length(series)
  if (is.null(m)) m <- if (n > 250) ceiling(n / 200) else 1
  if (is.null(s)) s <- 6 * m
  
  # Sequences calculation
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
  
  # Compute global average only if needed
  if (mode %in% c("all", "avg_with_test")) {
    series_cleaned <- series[is.finite(series) & complete.cases(series)]
    global_average <- mean(series_cleaned)
  } else {
    global_average <- NULL
  }
  
  # Function to process sub-series
  process_subseries <- function(ind) {
    center <- x_values[ind]
    len <- y_values[ind]
    start <- max(1, ceiling(center - (len - 1) / 2))
    end <- min(n, ceiling(center + (len - 1) / 2))
    
    # Extract and clean sub-series
    subseries <- series[start:end]
    subseries <- subseries[is.finite(subseries) & complete.cases(subseries)]
    
    # Initialize outputs
    mean_subserie <- t_test <- trend <- mk <- NA
    v7_flag <- v8_flag <- 2  # Default: not significant
    
    # Compute components based on mode
    if (mode %in% c("all", "avg_only", "avg_trend", "avg_with_test")) {
      if (length(subseries) >= s) {
        mean_subserie <- tryCatch(mean(subseries), error = function(e) NA)
      }
    }
    
    if (mode %in% c("all", "avg_with_test")) {
      if (length(subseries) >= s && length(subseries) >= 2) {
        t_test <- tryCatch(
          t.test(subseries, mu = global_average)$p.value, 
          error = function(e) NA
        )
        # Update flag based on t-test
        if (!is.na(t_test) && t_test <= alpha) {
          v7_flag <- 1
        }
      }
    }
    
    if (mode %in% c("all", "trend_only", "avg_trend", "trend_with_test")) {
      if (length(unique(subseries)) >= 2 && length(subseries) >= s) {
        trend <- tryCatch(
          lm(subseries ~ seq_along(subseries))$coefficients[2], 
          error = function(e) NA
        )
        
        if (mode %in% c("all", "trend_with_test")) {
          mk <- tryCatch(
            TH_MK_Trend(subseries)$p.value, 
            error = function(e) NA
          )
          # Update flag based on t-test
          if (!is.na(mk) && mk <= alpha) {
            v8_flag <- 1
          }
        }
      }
    }
    
    c(center, len, mean_subserie, t_test, trend, mk, v7_flag, v8_flag)
  }
  
  # Parallel processing
  results_list <- parallel::mclapply(
    1:LEN_MAX, 
    process_subseries, 
    mc.cores = parallel::detectCores() - 1
  )
  results <- do.call(rbind, results_list)
  
  # Manage empty case
  if (is.null(results) || nrow(results) == 0) {
    warning("No valid subseries due to lack of variability, (unique values < 4).")
    results <- data.frame(matrix(ncol = 8, nrow = 0))
    colnames(results) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
  } else {
    results <- as.data.frame(results)
    colnames(results) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
  }
  
  # Add attributes
  attr(results, "m") <- m
  attr(results, "s") <- s
  attr(results, "n") <- max(results$V2, na.rm = TRUE)
  attr(results, "alpha") <- alpha
  attr(results, "mode") <- mode
  
  return(results)
}