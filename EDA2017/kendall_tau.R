if (!require(pcaPP)) {
  install.packages("pcaPP")
  library(pcaPP)
}
library(pcaPP)

# Function to calculate the optimized Tau-Kendall test (O(n log n))
kendall_tau <- function(x, y, alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  
  if (length(x) != length(y)) {
    stop("Le due serie devono avere la stessa lunghezza.")
  }
  
  n <- length(x)
  
  tau <- cor.fk(x, y)
  
  var_tau <- (2 * (2 * n + 5)) / (9 * n * (n - 1))
  z <- tau / sqrt(var_tau)
  
  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(-abs(z))
  } else if (alternative == "greater") {
    p_value <- pnorm(-z)
  } else if (alternative == "less") {
    p_value <- pnorm(z)
  }
  
  return(list(tau = tau, p_value = p_value))
}
