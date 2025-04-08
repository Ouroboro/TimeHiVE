#' Perform Exploratory Data Analysis on Time Series
#'
#' @param series Numerical vector containing NA for missing values.
#' @param m Optional, scaling factor (autocalculated).
#' @param s Optional, cutoff series length (autocalculated: default 6*m).
#' @return A result DataFrame to pass to plotting_single().
#' @export
#' @importFrom parallel mclapply detectCores
#' @importFrom stats t.test lm pnorm

eda_single <- function(series, m = NULL, s = NULL) {
  
  # Check input
  if (missing(series)) stop("Input 'series' is required")
  
  # Manage input
  if(is.character(series)) {
    series <- read_ts_data(series)
  }
  
  # Define scales
  n <- length(series)
  if (is.null(m)) m <- if (n > 250) ceiling(n / 200) else 1
  s <- 6 * m
  
  m <- 1
  # Sequence calculation
  seq_x <- seq(s, n, by = m)
  seq_y <- seq(s / 2, n - s / 2, by = m)
  
  # Pre-allocate results
  LEN_MAX <- length(seq_x) * length(seq_y)
  x_values <- numeric(LEN_MAX)
  y_values <- numeric(LEN_MAX)
  results <- matrix(NA, ncol = 7, nrow = LEN_MAX)
  
  # Populate coordinates
  index <- 1
  for (len in seq_x) {
    for (y in seq(len / 2, n - len / 2, by = m)) {
      x_values[index] <- y
      y_values[index] <- len
      index <- index + 1
    }
  }
  
  series_cleaned <- series[is.finite(series) & complete.cases(series)]
  media_totale <- mean(series_cleaned)
  
  # Function to process subseries
  process_subseries <- function(ind) {
    centro <- x_values[ind]
    len <- y_values[ind]
    start <- max(1, round(centro - (len - 1) / 2))
    end <- min(n, ceiling(centro + (len - 1) / 2))
    
    # Extract and clean subseries
    sottoserie <- series[start:end]
    sottoserie <- sottoserie[is.finite(sottoserie) & complete.cases(sottoserie)]
    
    if (length(sottoserie) < s || length(unique(sottoserie)) < 4) return(NULL)
    
    mean_subserie <- mean(sottoserie)
    t_test <- tryCatch(t.test(sottoserie, mu = media_totale)$p.value, error = function(e) NA)
    trend <- tryCatch(lm(sottoserie ~ seq_along(sottoserie))$coefficients[2], error = function(e) NA)
    mk <- tryCatch(mann_kendall(sottoserie)$p.value, error = function(e) NA)
    
    c(centro, len, mean_subserie, t_test, trend, mk, ifelse(!is.na(t_test) && t_test <= 0.1, 1, 2))
  }
  
  # Parallel processing
  results_list <- mclapply(1:LEN_MAX, process_subseries, mc.cores = detectCores() - 1)
  results <- do.call(rbind, results_list)
  
  # Convert to data.frame
  results <- as.data.frame(results)
  colnames(results) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7")
  
  return(results)
}
