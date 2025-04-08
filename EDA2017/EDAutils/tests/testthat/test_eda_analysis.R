context("EDA Analysis")

test_that("Function works with vector input", {
  data <- rnorm(100)
  expect_silent(res <- eda_single(data, m = 10))
  expect_s3_class(res, "data.frame")
})

test_that("Handles file input", {
  tf <- tempfile(fileext = ".txt")
  writeLines(as.character(rnorm(100)), tf)
  
  expect_silent(res <- eda_single(tf, m = 5))
  expect_true(nrow(res) > 0)
  
  unlink(tf)
})