#' Perform hierarchical moving-window statistical analysis for time series.
#' @description
#' Perform hierarchical moving-window statistical analysis using custom functions.
#' It returns analysis results in a `data.frame` to be passed to TH_plott() function.
#'
#' @param ... User-defined functions for analysis
#' @param series A `list` of `numerical vectors` or a single `numerical vector`
#' @param m a `numeric` positive integer, subsampling parameter. Optional (default: autocalculated).
#' @param s a `numeric` positive integer, cutoff parameters for subseries length. Optional (default: autocalculated).
#' @param param a `numeric` significance level for flagging (default: 0.1).
#' @return A result `data.frame` to pass to TH_plott() with attributes.
#' @export
#' @importFrom parallel mclapply detectCores
#' @author Vladimiro Andrea Boselli, (2025) \email{boselli.v@@irea.cnr.it}
#' @examples
#'  \dontrun{
#' output <- TH_tweak(
#'   function(x, y) cor.test(x, y, method = "pearson")$estimate,
#'   function(x, y) cor.test(x, y, method = "kendall")$estimate,
#'   series = list(serie1, serie2)
#' )
#' }
TH_tweak <- function(..., 
                     series, 
                     m = NULL, 
                     s = NULL, 
                     param = 0.1) {
  
  # 1. Capture functions and series
  funs <- list(...)
  if (length(funs) == 0) stop("At least one function must be provided")
  
  # Convert to list if single series
  if (!is.list(series)) {
    series <- list(series)
  }
  
  # 2. Validate inputs
  n <- unique(sapply(series, length))
  if (length(n) > 1) stop("All series must have the same length")
  n <- n[1]
  
  # Initialize function variables with NA
  fun_vars <- setNames(
    rep(NA, length(funs)),
    paste0("fun", seq_along(funs))
  )
  
  # 3. Calculate parameters
  if (is.null(m)) m <- if (n > 250) ceiling(n / 200) else 1
  if (is.null(s)) s <- 6 * m
  
  # 4. Generate window sequences
  seq_x <- seq(s, n, by = m)
  
  # Calculate exact number of windows
  total_windows <- 0
  for (len in seq_x) {
    centers <- seq(from = max(len / 2, 1), to = min(n - len / 2, n), by = m)
    total_windows <- total_windows + length(centers)
  }
  
  x_values <- numeric(total_windows)
  y_values <- numeric(total_windows)
  
  # 5. Populate window coordinates
  index <- 1
  for (len in seq_x) {
    centers <- seq(from = max(len / 2, 1), to = min(n - len / 2, n), by = m)
    for (center in centers) {
      x_values[index] <- center
      y_values[index] <- len
      index <- index + 1
    }
  }
  
  # 6. Core computation function
  compute_window <- function(ind) {
    center <- x_values[ind]
    len <- y_values[ind]
    start <- max(1, ceiling(center - (len - 1) / 2))
    end <- min(n, ceiling(center + (len - 1) / 2))
    
    # Extract sub-series
    window_series <- lapply(series, function(s) {
      sub <- s[start:end]
      sub[is.finite(sub) & complete.cases(sub)]
    })
    
    # Apply functions and handle errors
    results <- sapply(funs, function(f) {
      tryCatch(
        do.call(f, window_series),
        error = function(e) {
          warning(paste("Error in function at window", ind, ":", e$message))
          NA
        }
      )
    })
    
    # Update function variables (optional)
    for (i in seq_along(funs)) {
      fun_vars[paste0("fun", i)] <<- if (is.na(results[i])) NA else "computed"
    }
    
    return(results)
  }
  
  # 7. Parallel execution
  results_list <- parallel::mclapply(
    1:total_windows,  # Cambiato da LEN_MAX a total_windows
    compute_window, 
    mc.cores = parallel::detectCores() - 1
  )
  
  # 8. Convert to data frame
  results_matrix <- do.call(rbind, results_list)
  colnames(results_matrix) <- paste0("F", seq_along(funs))
  
  # 9. Create final data frame
  results <- data.frame(
    center = x_values,
    length = y_values,
    results_matrix,
    stringsAsFactors = FALSE
  )
  
  # 10. Add attributes
  attr(results, "m") <- m
  attr(results, "s") <- s
  attr(results, "n") <- n
  attr(results, "param") <- param
  attr(results, "functions") <- sapply(substitute(list(...))[-1], deparse)
  attr(results, "fun_vars") <- fun_vars  # NA-initialized function variables
  
  return(results)
}