#' Mann-Kendall Trend Test
#' 
#' Efficient implementation of the Mann-Kendall test for trend detection using
#' merge sort algorithm (O(n log^2 n) complexity).
#' 
#' @param data_series Numeric vector of data to analyze
#' @return A list with components:
#' \item{S}{Mann-Kendall S statistic}
#' \item{Var.S}{Variance of S}
#' \item{Z}{Z-score}
#' \item{p.value}{Two-sided p-value}
#' @export
#' @examples
#' data <- c(1.2, 3.4, 2.5, 4.1, 5.6)
#' result <- mann_kendall(data)
mann_kendall <- function(data_series) {
  merge_sort_with_swaps <- function(vec) {
    # Initialize the trade count
    swap_count <- 0
    
    # Merge function
    merge <- function(left, right) {
      merged <- c()
      i <- 1
      j <- 1
      
      while (i <= length(left) && j <= length(right)) {
        if (left[i] <= right[j]) {
          merged <- c(merged, left[i])
          i <- i + 1
        } else {
          merged <- c(merged, right[j])
          j <- j + 1
          swap_count <<- swap_count + (length(left) - i + 1)
        }
      }
      
      # Add missing elements
      if (i <= length(left)) {
        merged <- c(merged, left[i:length(left)])
      }
      if (j <= length(right)) {
        merged <- c(merged, right[j:length(right)])
      }
      
      return(merged)
    }
    
    # Recursive function for merge sort
    merge_sort <- function(vec) {
      if (length(vec) <= 1) {
        return(vec)
      }
      
      mid <- floor(length(vec) / 2)
      left <- merge_sort(vec[1:mid])
      right <- merge_sort(vec[(mid + 1):length(vec)])
      
      return(merge(left, right))
    }
    
    # Sort the vector and calculate the number of exchanges
    merge_sort(vec)
    
    # Return the number of trades
    return(swap_count)
  }
  
  count_occurrences <- function(vec) {
    # Initialize a list to store the counts
    counts <- list()
    
    # Count the occurrences of each element
    for (val in vec) {
      if (!is.null(counts[[as.character(val)]])) {
        counts[[as.character(val)]] <- counts[[as.character(val)]] + 1
      } else {
        counts[[as.character(val)]] <- 1
      }
    }
    
    # Filter occurrences greater than 1
    result <- unlist(counts[counts > 1])
    
    # Return the vector with occurrences
    return(result)
  }
  
  n <- length(data_series)
  
  ### MATH CORE TO COMPUTE S
  P <- merge_sort_with_swaps(data_series) #Number of exchanges to order the vector
  O <- count_occurrences(data_series) #Number of repeated elements
  adjO <- sum((O*(O-1)/2))
  S <- n*(n-1)/2 - adjO - P*2
  ###
  
  VarS <- (n * (n - 1) * (2 * n + 5)) / 18
  
  if (S >= 0) {
    Z <- (S - 1) / sqrt(VarS)
  } else {
    Z <- (S + 1) / sqrt(VarS)
  }
  
  p.value <- 0.5 - abs(0.5 - pnorm(Z))
  
  return(list(S = S, Var.S = VarS, Z = Z, p.value = p.value))
}