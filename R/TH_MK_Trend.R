#' Mann-Kendall Trend Test with Knight's Algorithm
#' @description
#' Efficient implementation of the Mann-Kendall test for trend detection using
#' merge sort algorithm (O(n log n) complexity).
#'
#' @param data_series a `numerical vector` containing NA for missing values.
#' @return A list with components:
#' \item{S}{Mann-Kendall S statistic}
#' \item{Var.S}{Variance of S}
#' \item{Z}{Z-score}
#' \item{p.value}{Two-sided p-value}
#' @export
#' @author Vladimiro Andrea Boselli, (2025) \email{boselli.v@@irea.cnr.it}
#' @examples
#' \dontrun{
#' data <- c(1.2, 3.4, 2.5, 4.1, 5.6)
#' result <- TH_MK_Trend(data)
#' }

TH_MK_Trend <- function(data_series) {
  #Clean NA
  data_series <- data_series[!is.na(data_series)]
  n <- length(data_series)
  
  ### MERGE SORT
  
  count_inversions <- function(arr) {
    # Count Inversions
    merge_count <- function(arr, l, m, r) {
      left <- arr[l:m]
      right <- arr[(m+1):r]
      i <- j <- 1
      k <- l
      inv_count <- 0
      left_len <- length(left)
      right_len <- length(right)
      
      while (i <= left_len && j <= right_len) {
        if (left[i] <= right[j]) {
          arr[k] <- left[i]
          i <- i + 1
        } else {
          arr[k] <- right[j]
          j <- j + 1
          inv_count <- inv_count + (left_len - i + 1)
        }
        k <- k + 1
      }
      
      while (i <= left_len) {
        arr[k] <- left[i]
        i <- i + 1
        k <- k + 1
      }
      
      while (j <= right_len) {
        arr[k] <- right[j]
        j <- j + 1
        k <- k + 1
      }
      
      list(arr = arr, inv = inv_count)
    }
    
    # Recursive function for merge sort
    merge_sort_count <- function(arr, l, r) {
      inv_total <- 0
      
      if (l < r) {
        m <- floor((l + r) / 2)
        
        # Order
        left_res <- merge_sort_count(arr, l, m)
        arr <- left_res$arr
        inv_total <- inv_total + left_res$inv
        
        right_res <- merge_sort_count(arr, m+1, r)
        arr <- right_res$arr
        inv_total <- inv_total + right_res$inv
        
        # Merge
        merge_res <- merge_count(arr, l, m, r)
        arr <- merge_res$arr
        inv_total <- inv_total + merge_res$inv
      }
      
      list(arr = arr, inv = inv_total)
    }
    
    result <- merge_sort_count(arr, 1, length(arr))
    return(result$inv)
  }
  
  ### COUNT OCCURENCIES
  
  count_occurrences <- function(vec) {
    # Order (O(n log n))
    sorted_vec <- sort(vec)
    
    # Count (O(n))
    runs <- rle(sorted_vec)$lengths
    
    # Filter
    result <- runs[runs > 1]
    
    return(result)
  }
  
  ### STATS
  
  P <- count_inversions(data_series)
  O <- count_occurrences(data_series)
  adjO <- sum(O * (O - 1) / 2)
  S <- n * (n - 1) / 2 - adjO - 2 * P
  
  VarS <- (n * (n - 1) * (2 * n + 5)) / 18
  
  if (S > 0) {
    Z <- (S - 1) / sqrt(VarS)
  } else if (S < 0) {
    Z <- (S + 1) / sqrt(VarS)
  } else {
    Z <- 0
  }
  
  p.value <- 0.5 - abs(0.5 - pnorm(Z))
  
  return(list(S = S, Var.S = VarS, Z = Z, p.value = p.value))
}