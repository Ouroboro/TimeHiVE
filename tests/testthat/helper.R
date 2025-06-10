# Funzioni di supporto per i test
create_test_series <- function(n = 100) {
  rnorm(n)
}

save_test_file <- function(data, filename) {
  writeLines(as.character(data), filename)
}