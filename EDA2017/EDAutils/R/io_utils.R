#' Read Time Series Data from File
#'
#' @param file_path Path to the input file
#' @param ... Additional arguments passed to `scan()`
#' @return Numeric vector with the data
#' @export
read_ts_data <- function(file_path, ...) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  scan(file_path, what = numeric(), quiet = TRUE, ...)
}