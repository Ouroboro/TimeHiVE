context("Mann-Kendall Test")

test_that("Basic functionality", {
  data <- c(1.2, 3.4, 2.5, 4.1, 5.6)
  result <- mann_kendall(data)
  
  expect_type(result, "list")
  expect_named(result, c("S", "Var.S", "Z", "p.value"))
})

test_that("Handles file input", {
  # Crea un file temporaneo per il test
  tf <- tempfile(fileext = ".txt")
  writeLines(as.character(c(1.1, 2.3, 1.8, 2.9, 3.5)), tf)
  
  expect_silent(res <- mann_kendall(tf))
  expect_true(is.list(res))
  
  unlink(tf)  # Pulizia
})