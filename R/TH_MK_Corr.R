#' Mann-Kendall Tau Test with Knight's Algorithm
#' @description
#' Efficient implementation of the Mann-Kendall test for correlation using
#' Knight's algorithm of (O(n log n) complexity). Handles ties correctly.
#'
#' @param x a `numerical vector`
#' @param y a `numerical vector`
#' @param alternative Type of alternative hypothesis ("two.sided", "greater", "less")
#' @return A `htest` object with results
#' \item{statistic}{Kendall's tau-b statistic}
#' \item{p.value}{p-value}
#' \item{alternative}{Alternative hypothesis description}
#' @export
#' @author Vladimiro Andrea Boselli, (2025) \email{boselli.v@@irea.cnr.it}
#' @examples
#' \dontrun{
#' kt <- TH_MK_Corr(sub_series1, sub_series2, alternative = "less")
#' c(kt$statistic, kt$p.value)
#' }

TH_MK_Corr <- function(x, y, alternative = c("two.sided", "greater", "less")) {
  
  # Validate inputs
  alternative <- match.arg(alternative)
  complete <- complete.cases(x, y)
  x <- x[complete]
  y <- y[complete]
  n <- length(x)
  if (n < 2) stop("At least 2 complete cases required")
  if (var(x) == 0 || var(y) == 0) warning("One vector has zero variance - results may be invalid")
  
  # Efficient inversion counting with iterative merge sort (handles ties)
  count_inversions <- function(y) {
    n <- length(y)
    if (n <= 1) return(0)
    
    # Initialize workspace and counters
    inv_count <- 0
    work_arr <- numeric(n)
    
    # Merge function with inversion counting
    merge_count <- function(arr, l, m, r) {
      left <- arr[l:m]
      right <- arr[(m+1):r]
      i <- j <- 0
      k <- l
      inv <- 0
      
      while (i < length(left) && j < length(right)) {
        if (left[i+1] <= right[j+1]) {
          arr[k] <- left[i+1]
          i <- i + 1
        } else {
          arr[k] <- right[j+1]
          j <- j + 1
          # Count inversions: all remaining left elements > current right
          inv <- inv + (length(left) - i)
        }
        k <- k + 1
      }
      
      # Add remaining elements
      while (i < length(left)) {
        arr[k] <- left[i+1]
        i <- i + 1
        k <- k + 1
      }
      while (j < length(right)) {
        arr[k] <- right[j+1]
        j <- j + 1
        k <- k + 1
      }
      list(arr = arr, inv = inv)
    }
    
    # Iterative merge sort
    curr_size <- 1
    inv_total <- 0
    arr <- y
    while (curr_size < n) {
      l <- 1
      while (l < n) {
        m <- min(l + curr_size - 1, n)
        r <- min(l + 2*curr_size - 1, n)
        if (m < r) {
          res <- merge_count(arr, l, m, r)
          arr <- res$arr
          inv_total <- inv_total + res$inv
        }
        l <- l + 2*curr_size
      }
      curr_size <- 2*curr_size
    }
    return(inv_total)
  }
  
  # Compute tied groups efficiently
  compute_tied_groups <- function(vec) {
    sorted <- sort(vec)
    runs <- rle(sorted)$lengths
    runs[runs > 1]  # Only return groups with ties
  }
  
  # Order by x and break ties with y
  ord <- order(x, y)
  y_ordered <- y[ord]
  
  # Count discordant pairs (inversions in y-order)
  Q <- count_inversions(y_ordered)
  
  # Calculate tied groups
  tx <- compute_tied_groups(x)
  ty <- compute_tied_groups(y)
  
  # Tied groups for (x,y) pairs
  z <- paste(x, y, sep = "|")
  txy <- compute_tied_groups(z)
  
  # Compute correction terms for variance
  T_x <- sum(tx * (tx - 1) / 2)
  T_y <- sum(ty * (ty - 1) / 2)
  T_xy <- sum(txy * (txy - 1) / 2)
  
  # Total pairs and comparable pairs
  total_pairs <- n * (n - 1) / 2
  comparable_pairs <- total_pairs - (T_x + T_y - T_xy)
  
  # Kendall's S statistic and tau-b
  S <- comparable_pairs - 2 * Q
  denom_x <- total_pairs - T_x
  denom_y <- total_pairs - T_y
  tau_b <- if (denom_x > 0 && denom_y > 0) {
    S / sqrt(denom_x * denom_y)
  } else 0
  
  # Variance with tie correction
  term1 <- n * (n - 1) * (2 * n + 5)
  term2 <- if (length(tx) > 0) sum(tx * (tx - 1) * (2 * tx + 5)) else 0
  term3 <- if (length(ty) > 0) sum(ty * (ty - 1) * (2 * ty + 5)) else 0
  term4 <- if (length(tx) > 0) sum(tx * (tx - 1) * (tx - 2)) else 0
  term5 <- if (length(ty) > 0) sum(ty * (ty - 1) * (ty - 2)) else 0
  term6 <- if (length(tx) > 0) sum(tx * (tx - 1)) else 0
  term7 <- if (length(ty) > 0) sum(ty * (ty - 1)) else 0
  
  varS <- (term1 - term2 - term3) / 18 +
    (term4 * term5) / (9 * n * (n - 1) * (n - 2)) +
    (term6 * term7) / (2 * n * (n - 1))
  
  # Normal approximation
  if (varS > 0) {
    z_score <- S / sqrt(varS)
  } else {
    z_score <- 0
  }
  
  # Calculate p-value based on alternative
  p_value <- switch(alternative,
                    "two.sided" = 2 * pnorm(-abs(z_score)),
                    "greater" = pnorm(z_score, lower.tail = FALSE),
                    "less" = pnorm(z_score)
  )
  
  # Build htest object
  method <- "Kendall's rank correlation tau-b with tie correction"
  alt_text <- switch(alternative,
                     "two.sided" = "true tau is not equal to 0",
                     "greater" = "true tau is greater than 0",
                     "less" = "true tau is less than 0"
  )
  
  structure(
    list(
      statistic = c(tau = tau_b),
      p.value = p_value,
      alternative = alt_text,
      method = method,
      data.name = paste(deparse(substitute(x)), "and", deparse(substitute(y))),
      parameter = c(n = n, comparable_pairs = comparable_pairs),
      estimate = c(S = S, varS = varS)
    ),
    class = "htest"
  )
}