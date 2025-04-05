#' Perform Exploratory Data Analysis on Time Series
#'
#' @param series Numeric vector of time series data
#' @param m Integer, window step parameter (default: auto-calculated)
#' @param s Integer, scale parameter (default: 6*m)
#' @param parallel Logical, whether to use parallel computation (default: TRUE)
#' @return A data.frame with analysis results
#' @export
#' @importFrom parallel mclapply detectCores
#' @importFrom stats t.test lm pnorm
eda_analyze <- function(series, m = NULL, s = NULL, parallel = TRUE) {
  n <- length(series)
  
  # Calcolo parametri automatici se non forniti
  if (is.null(m)) m <- if (n > 250) ceiling(n / 200) else 1
  if (is.null(s)) s <- 6 * m
  
  scale <- 200 / n * m
  
  # Generate x and y value sequences
  seq_x <- seq(s, n, by = m)
  seq_y <- seq(s / 2, n - s / 2, by = m)
  
  # Pre-compute lengths to reduce repetitive calculations
  len_x <- length(seq_x)
  len_y <- length(seq_y)
  LEN_MAX <- len_x * len_y
  
  # Pre-allocate vectors and matrix for results
  x_values <- numeric(LEN_MAX)
  y_values <- numeric(LEN_MAX)
  results <- matrix(NA, ncol = 7, nrow = LEN_MAX)
  
  # Populate x_values and y_values
  index <- 1
  for (len in seq_x) {
    for (y in seq(len / 2, n - len / 2, by = m)) {
      x_values[index] <- y
      y_values[index] <- len
      index <- index + 1
    }
  }
  
  # Compute global mean of the series, filtering invalid values
  serie_cleaned <- serie[is.finite(serie) & complete.cases(serie)]
  media_totale <- mean(serie_cleaned)
  
  # Function to process sub-series and populate the results matrix
  process_subseries <- function(ind) {
    centro <- x_values[ind]
    len <- y_values[ind]
    start <- max(1, round(centro - (len - 1) / 2))
    end <- min(n, ceiling(centro + (len - 1) / 2))
    
    # Extract and clean sub-series
    sottoserie <- serie[start:end]
    sottoserie <- sottoserie[is.finite(sottoserie) & complete.cases(sottoserie)]
    
    # Skip small or repetitive sub-series
    if (length(sottoserie) < s || length(unique(sottoserie)) < 4) return(NULL)
    
    # Compute values
    mean_subserie <- mean(sottoserie)
    t_test <- tryCatch(t.test(sottoserie, mu = media_totale)$p.value, error = function(e) NA)
    trend <- tryCatch(lm(sottoserie ~ seq_along(sottoserie))$coefficients[2], error = function(e) NA)
    mk <- tryCatch(mann_kendall(sottoserie)$p.value, error = function(e) NA)
    
    # Return row for the results matrix
    c(centro, len, mean_subserie, t_test, trend, mk, ifelse(!is.na(t_test) && t_test <= 0.1, 1, 2))
  }
  
  # Parallelize computation for efficiency
  library(parallel)
  results_list <- mclapply(1:LEN_MAX, process_subseries, mc.cores = detectCores() - 1)
  results <- do.call(rbind, results_list)
  
  # Convert results to a data frame
  results <- as.data.frame(results)
  colnames(results) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7")
  
  return(results)
}