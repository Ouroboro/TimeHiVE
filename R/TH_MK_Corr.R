#' Mann-Kendall Tau Test with Knight's Algorithm
#' @description
#' Efficient implementation of the Mann-Kendall test for correlation using
#' Knight's algorithm of (O(n log n) complexity).
#'
#' @param `x` a `numerical vector`.
#' @param `x` a `numerical vector`.
#' @param `alternative` Type of alternative hypothesis ("two.sided", "greater", "less")
#' @return A `htest` object with results
#' \item{statistic}{Mann-Kendall tau statistic}
#' \item{p.value}{p-value}
#' \item{alt}{Alternative Hypotesis}
#' @export
#' @author Vladimiro Andrea Boselli, (2025) \email{boselli.v@@irea.cnr.it}
#' @examples
#'  \dontrun{
#` kt <- TH_MK_Corr(sub_series1, sub_series2, alternative = "less")
#` c(kt$statistic, kt$p.value)
#' }
### function TH_MK_Corr

TH_MK_Corr <- function(x, y, alternative = c("two.sided", "greater", "less")) {
  
  ### COMPUTE KENDALL TAU-A
  
  compute_kendall_tau <- function(x_rank, y_rank, n) {
    inv <- count_inversions(x_rank, y_rank)
    total_pairs <- n * (n - 1) / 2
    return(1 - 2 * inv / total_pairs)
  }
  
  ### COUNT INVERSIONS IN O(n Log n)
  
  count_inversions <- function(x_rank, y_rank) {
    n <- length(x_rank)
    if (n != length(y_rank)) stop("Vectors must have same length")
    
    # Order pairs by x_rank, then by y_rank
    ord <- order(x_rank, y_rank)
    y_sorted <- y_rank[ord]
    
    # Helper function for merge sort with inversion counting
    merge_sort_count <- function(arr) {
      if (length(arr) <= 1) return(list(sorted = arr, inversions = 0))
      
      mid <- length(arr) %/% 2
      left <- merge_sort_count(arr[1:mid])
      right <- merge_sort_count(arr[(mid+1):length(arr)])
      
      merged <- numeric(length(arr))
      i <- j <- k <- 0
      inv <- left$inversions + right$inversions
      
      while (i < length(left$sorted) && j < length(right$sorted)) {
        if (left$sorted[i+1] <= right$sorted[j+1]) {
          merged[k+1] <- left$sorted[i+1]
          i <- i + 1
        } else {
          merged[k+1] <- right$sorted[j+1]
          j <- j + 1
          inv <- inv + (length(left$sorted) - i)  # Count all inversions
        }
        k <- k + 1
      }
      
      # Add remaining elements
      merged[(k+1):length(merged)] <- if (i < length(left$sorted)) left$sorted[(i+1):length(left$sorted)] else right$sorted[(j+1):length(right$sorted)]
      
      list(sorted = merged, inversions = inv)
    }
    
    # Execute and return count
    result <- merge_sort_count(y_sorted)
    result$inversions
  }
  
  ### VALIDATE AND RANK
  
  validate_and_rank <- function(x, y) {
    # Convert to vectors if needed
    x <- as.vector(x)
    y <- as.vector(y)
    
    # Check types
    if (!is.numeric(x) || !is.numeric(y)) {
      stop("Both inputs must be numeric vectors")
    }
    
    # Check lengths
    if (length(x) != length(y)) {
      stop("Vectors must have the same length")
    }
    
    # Handle missing values
    complete <- complete.cases(x, y)
    x <- x[complete]
    y <- y[complete]
    
    # Check minimum length
    if (length(x) < 2) {
      stop("At least 2 complete cases required")
    }
    
    # Check variance
    if (var(x) == 0 || var(y) == 0) {
      warning("One of the vectors has zero variance")
      return(NULL)
    }
    
    return(list(
      x_rank = rank(x),
      y_rank = rank(y),
      n = length(x)
    ))
  }
  
  ### COMPUTE KENDALL P-VALUE
  
  compute_kendall_pvalue <- function(tau, n, alternative = c("two.sided", "greater", "less")) {
    alternative <- match.arg(alternative)
    
    # Standard normal approximation for tau-a
    S <- tau / sqrt(2 * (2 * n + 5) / (9 * n * (n - 1)))
    
    switch(alternative,
           "two.sided" = 2 * pnorm(-abs(S)),
           "greater" = pnorm(S, lower.tail = FALSE),
           "less" = pnorm(S))
  }
  
  ### START HERE TH_MK_Corr() FUNCTION
  
  alternative <- match.arg(alternative)
  
  ranked <- validate_and_rank(x, y)
  if (is.null(ranked)) return(NA_real_)
  
  # Calculate correlation
  tau <- compute_kendall_tau(ranked$x_rank, ranked$y_rank, ranked$n)
  
  # Calculate p-value
  p_value <- compute_kendall_pvalue(tau, ranked$n, alternative)
  
  # Prepare result
  method <- "Kendall's rank correlation tau-a"
  alt <- switch(alternative,
                "two.sided" = "true tau is not equal to 0",
                "greater" = "true tau is greater than 0",
                "less" = "true tau is less than 0")
  
  structure(
    list(
      statistic = c(tau = tau),
      p.value = p_value,
      alternative = alt,
      method = method,
      data.name = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    ),
    class = "htest"
  )
}