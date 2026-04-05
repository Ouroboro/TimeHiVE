# tests/testthat/test-TH_functions.R

library(testthat)
#library(ggplot2)
library(gridExtra)

# ----------------------------------------------------------------------
# Test environment configuration
# ----------------------------------------------------------------------

# Force single-core usage to avoid issues with mclapply in R CMD check
# (useful if tests are not skipped for some reason)
options(mc.cores = 1)

# ----------------------------------------------------------------------
# Example data
# ----------------------------------------------------------------------
set.seed(123)
n <- 100
series1 <- cumsum(rnorm(n))
series2 <- series1 + rnorm(n, 0, 2)   # correlated
series_flat <- rep(5, n)               # constant, no trend
series_na <- series1
series_na[c(10, 20, 30)] <- NA
series_short <- rnorm(10)

# ----------------------------------------------------------------------
# Tests for TH_MK_Trend (no parallelization)
# ----------------------------------------------------------------------
test_that("TH_MK_Trend works correctly", {
  x <- 1:50
  res <- TH_MK_Trend(x)
  expect_type(res, "list")
  expect_named(res, c("S", "Var.S", "Z", "p.value"))
  expect_equal(res$S, 1225)
  expect_true(res$p.value < 0.05)
  
  x <- rnorm(50)
  res <- TH_MK_Trend(x)
  expect_true(abs(res$Z) < 2)
  
  x <- c(1,1,2,2,3,3,4,4,5,5)
  res <- TH_MK_Trend(x)
  expect_true(is.finite(res$S))
  
  x <- rep(1, 10)
  res <- TH_MK_Trend(x)
  expect_equal(res$S, 0)
  expect_equal(res$p.value, 0.5)
  
  x <- c(1,2,NA,4,5)
  res <- TH_MK_Trend(x)
  expect_false(any(is.na(res)))
})

# ----------------------------------------------------------------------
# Tests for TH_MK_Corr (no parallelization)
# ----------------------------------------------------------------------
test_that("TH_MK_Corr works correctly", {
  x <- 1:20
  y <- x
  res <- TH_MK_Corr(x, y, alternative = "two.sided")
  expect_s3_class(res, "htest")
  expect_equal(res$statistic[["tau"]], 1)
  
  y <- rev(x)
  res <- TH_MK_Corr(x, y, alternative = "two.sided")
  expect_equal(res$statistic[["tau"]], -1)
  
  x <- c(1,1,2,2,3,3)
  y <- c(2,2,1,1,3,3)
  res <- TH_MK_Corr(x, y)
  expect_true(is.finite(res$statistic))
  
  x <- rep(1,10)
  y <- 1:10
  expect_warning(res <- TH_MK_Corr(x, y))
  expect_true(is.finite(res$statistic))
  
  x <- 1:1
  y <- 1:1
  expect_error(TH_MK_Corr(x, y), "At least 2 complete cases required")
  
  x <- c(1,2,NA,4,5)
  y <- c(5,4,NA,2,1)
  res <- TH_MK_Corr(x, y)
  expect_false(anyNA(res$statistic))
})

# ----------------------------------------------------------------------
# Tests for TH_coupled (uses parallelization)
# ----------------------------------------------------------------------
test_that("TH_coupled basic functionality", {
  skip_on_ci()  # Skip on GitHub Actions and other CI to avoid parallelization errors
  
  res <- TH_coupled(series1, series2, m = 2, s = 6, alpha = 0.05)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8"))
  expect_true(nrow(res) > 0)
  expect_equal(attr(res, "m"), 2)
  expect_equal(attr(res, "s"), 6)
  expect_equal(attr(res, "n"), length(series1))
  expect_equal(attr(res, "alpha"), 0.05)
  expect_equal(attr(res, "mode"), "all")
  
  res_pearson <- TH_coupled(series1, series2, mode = "pearson")
  expect_true(all(c("V3", "V4") %in% names(res_pearson)))
  expect_true(all(is.na(res_pearson$V5)))
  
  res_kendall <- TH_coupled(series1, series2, mode = "kendall")
  expect_true(all(is.na(res_kendall$V3)))
  
  res_both <- TH_coupled(series1, series2, mode = "both")
  expect_false(all(is.na(res_both$V3)))
  expect_false(all(is.na(res_both$V5)))
  
  # Very short series: provide valid m and s (s <= n) to avoid length validation error
  short <- 1:5
  res_short <- TH_coupled(short, short, m = 1, s = 3)
  expect_true(nrow(res_short) > 0)
  expect_true(is.numeric(res_short$V3) || all(is.na(res_short$V3)))
  
  # When m and s are omitted with n=5, default s=6 triggers length validation error
  expect_error(
    TH_coupled(short, short),
    "Series length \\(5\\) is too short for analysis"
  )
  
  expect_error(TH_coupled(1:10, 1:5), "Series must have the same length")
  expect_error(TH_coupled(series1, series2, mode = "invalid"), "Invalid mode")
})

# ----------------------------------------------------------------------
# Tests for TH_single (uses parallelization)
# ----------------------------------------------------------------------
test_that("TH_single basic functionality", {
  skip_on_ci()
  
  res <- TH_single(series1, m = 2, s = 6, alpha = 0.05)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8"))
  expect_true(nrow(res) > 0)
  expect_equal(attr(res, "m"), 2)
  expect_equal(attr(res, "s"), 6)
  expect_equal(attr(res, "n"), length(series1))
  expect_equal(attr(res, "alpha"), 0.05)
  expect_equal(attr(res, "mode"), "all")
  
  res_avg <- TH_single(series1, mode = "avg_only")
  expect_false(all(is.na(res_avg$V3)))
  expect_true(all(is.na(res_avg$V5)))
  
  res_trend <- TH_single(series1, mode = "trend_only")
  expect_true(all(is.na(res_trend$V3)))
  expect_false(all(is.na(res_trend$V5)))
  
  res_const <- TH_single(series_flat)
  expect_true(nrow(res_const) > 0)
  
  res_na <- TH_single(series_na)
  expect_true(nrow(res_na) > 0)
  
  # Very short series: provide valid m and s (s <= n)
  res_short <- TH_single(series_short, m = 1, s = 3)
  expect_true(is.data.frame(res_short))
  
  # Empty series triggers length validation error
  empty <- numeric(0)
  expect_error(
    TH_single(empty),
    "Series length \\(0\\) is too short for analysis"
  )
})

# ----------------------------------------------------------------------
# Tests for TH_tweak (uses parallelization)
# ----------------------------------------------------------------------
test_that("TH_tweak basic functionality", {
  skip_on_ci()
  
  fun1 <- function(x, y) mean(x) - mean(y)
  fun2 <- function(x, y) sd(x) - sd(y)
  res <- TH_tweak(fun1, fun2, series = list(series1, series2), m = 2, s = 6, param = 0.1)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("center", "length", "F1", "F2"))
  expect_true(nrow(res) > 0)
  expect_equal(attr(res, "m"), 2)
  expect_equal(attr(res, "s"), 6)
  expect_equal(attr(res, "n"), length(series1))
  expect_equal(attr(res, "param"), 0.1)
  expect_equal(length(attr(res, "functions")), 2)
  
  # Test with single series using a function that accepts one argument
  fun_single <- function(x) mean(x)
  res_single <- TH_tweak(fun_single, series = series1, m = 2, s = 6)
  expect_named(res_single, c("center", "length", "F1"))
  expect_true(nrow(res_single) > 0)
  
  expect_error(TH_tweak(series = series1), "At least one function must be provided")
  
  # Test length validation for TH_tweak
  short_series <- list(rnorm(3), rnorm(3))
  expect_error(
    TH_tweak(fun1, series = short_series),
    "All series have length \\(3\\) which is too short for analysis"
  )
  
  # Function that errors: we expect NA values (warnings may be suppressed in parallel)
  fun_error <- function(x, y) stop("error")
  res_err <- suppressWarnings(TH_tweak(fun_error, series = list(series1, series2)))
  expect_true(all(is.na(res_err$F1)))
})

# ----------------------------------------------------------------------
# Tests for TH_plott (depends on TH_tweak)
# ----------------------------------------------------------------------
test_that("TH_plott returns a ggplot/grid object", {
  skip_on_ci()  # Skip because it depends on TH_tweak
  
  fun1 <- function(x, y) mean(x) - mean(y)
  fun2 <- function(x, y) cor(x, y, method = "pearson")
  res <- TH_tweak(fun1, fun2, series = list(series1, series2), m = 2, s = 6)
  
  p <- TH_plott(res)
  expect_s3_class(p, c("arrange", "gtable", "grob", "gDesc"))
  
  p2 <- TH_plott(res,
                 colorscales = list(c("blue","white","red"), "avg"),
                 colorlimits = list(c(-5,5), c(-1,1)))
  expect_s3_class(p2, c("arrange", "gtable", "grob", "gDesc"))
  
  tmp <- tempfile(fileext = ".png")
  p3 <- TH_plott(res, output_file = tmp)
  expect_true(file.exists(tmp))
  unlink(tmp)
  
  # Dummy object without function columns (should give error "No plots generated")
  bad_res <- data.frame(center = 1:5, length = 1:5)
  attr(bad_res, "n") <- 100
  attr(bad_res, "m") <- 1
  attr(bad_res, "s") <- 6
  attr(bad_res, "param") <- 0.1
  attr(bad_res, "functions") <- "test"
  expect_error(TH_plott(bad_res), "No plots generated")
})

# ----------------------------------------------------------------------
# Tests for TH_plots (depends on TH_single)
# ----------------------------------------------------------------------
test_that("TH_plots returns a ggplot/grid object", {
  skip_on_ci()
  
  res <- TH_single(series1, m = 2, s = 6, mode = "all")
  
  p <- TH_plots(res)
  expect_s3_class(p, c("arrange", "gtable", "grob", "gDesc"))
  
  p_mask <- TH_plots(res, mask = TRUE)
  expect_s3_class(p_mask, c("arrange", "gtable", "grob", "gDesc"))
  
  # avg_only mode with mask = TRUE: we expect a warning that mask is ignored
  res_avg <- TH_single(series1, mode = "avg_only")
  expect_warning(
    p_avg <- TH_plots(res_avg, mask = TRUE),
    "Mask parameter ignored for current mode"
  )
  expect_s3_class(p_avg, "ggplot")
  
  res_trend <- TH_single(series1, mode = "trend_only")
  p_trend <- TH_plots(res_trend)
  expect_s3_class(p_trend, "ggplot")
  
  tmp <- tempfile(fileext = ".png")
  p_save <- TH_plots(res, output_file = tmp)
  expect_true(file.exists(tmp))
  unlink(tmp)
  
  expect_error(TH_plots(res, mode = "invalid"), "Invalid mode")
})

# ----------------------------------------------------------------------
# Tests for TH_plotc (depends on TH_coupled)
# ----------------------------------------------------------------------
test_that("TH_plotc returns a ggplot/grid object", {
  skip_on_ci()
  
  res <- TH_coupled(series1, series2, m = 2, s = 6, mode = "all")
  
  p <- TH_plotc(res)
  expect_s3_class(p, c("arrange", "gtable", "grob", "gDesc"))
  
  p_mask <- TH_plotc(res, mask = TRUE)
  expect_s3_class(p_mask, c("arrange", "gtable", "grob", "gDesc"))
  
  res_pear <- TH_coupled(series1, series2, mode = "pearson")
  expect_warning(p_pear <- TH_plotc(res_pear, mask = TRUE),
                 "Mask parameter ignored for current mode")
  expect_s3_class(p_pear, "ggplot")
  
  res_kendall <- TH_coupled(series1, series2, mode = "kendall")
  p_kendall <- TH_plotc(res_kendall)
  expect_s3_class(p_kendall, "ggplot")
  
  tmp <- tempfile(fileext = ".png")
  p_save <- TH_plotc(res, output_file = tmp)
  expect_true(file.exists(tmp))
  unlink(tmp)
  
  expect_error(TH_plotc(res, mode = "invalid"), "Invalid mode")
})

# ----------------------------------------------------------------------
# Test for automatic calculation of m and s (depends on TH_single and TH_coupled)
# ----------------------------------------------------------------------
test_that("Auto-calculation of m and s works", {
  skip_on_ci()
  
  long_series <- rnorm(300)
  res_long <- TH_single(long_series)
  expect_equal(attr(res_long, "m"), 2)
  expect_equal(attr(res_long, "s"), 12)
  
  short_series <- rnorm(50)
  res_short <- TH_single(short_series)
  expect_equal(attr(res_short, "m"), 1)
  expect_equal(attr(res_short, "s"), 6)
  
  res_c_long <- TH_coupled(long_series, long_series)
  expect_equal(attr(res_c_long, "m"), 2)
  expect_equal(attr(res_c_long, "s"), 12)
})

# ----------------------------------------------------------------------
# Test for length validation (new tests)
# ----------------------------------------------------------------------

test_that("TH_single fails gracefully with very short series", {
  short_series <- rnorm(3)
  
  # Should get custom error message, not seq() error
  expect_error(
    TH_single(short_series),
    "Series length \\(3\\) is too short for analysis"
  )
  
  # Works with explicit m and s that satisfy the constraint
  expect_silent(
    TH_single(short_series, m = 1, s = 3)
  )
})

test_that("TH_coupled fails gracefully with very short series", {
  short_series1 <- rnorm(3)
  short_series2 <- rnorm(3)
  
  # Should get custom error message, not seq() error
  expect_error(
    TH_coupled(short_series1, short_series2),
    "Series length \\(3\\) is too short for analysis"
  )
  
  # Works with explicit m and s that satisfy the constraint
  expect_silent(
    TH_coupled(short_series1, short_series2, m = 1, s = 3)
  )
})

test_that("TH_tweak fails gracefully with very short series", {
  short_series <- list(rnorm(3), rnorm(3))
  fun_test <- function(x, y) mean(x) - mean(y)
  
  # Should get custom error message
  expect_error(
    TH_tweak(fun_test, series = short_series),
    "All series have length \\(3\\) which is too short for analysis"
  )
  
  # Works with explicit m and s that satisfy the constraint
  expect_silent(
    TH_tweak(fun_test, series = short_series, m = 1, s = 3)
  )
})