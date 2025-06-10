#' Create EDA Analysis Plots
#'
#' @param results Data.frame from eda_analyze().
#' @param scale Numeric, scaling factor for point sizes, autoset by default.
#' @param output_file Optional, filename to save plot (NULL to skip saving).
#' @return A gridExtra arranged plot object.
#' @export
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices colorRampPalette

eda_plots <- function(results, scale = NULL, output_file = NULL) {
  # Input validation
  if (!is.data.frame(results)) stop("results must be a data.frame")
  required_cols <- c("V1", "V2", "V3", "V4", "V5", "V6")
  if (!all(required_cols %in% names(results))) {
    stop("results must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Calculate dynamic parameters
  n <- max(results$V1, na.rm = TRUE)
  m_calculated <- if (n > 250) ceiling(n / 200) else 1
  
  if (is.null(scale)) {
    scale <- 200 / n * m_calculated
  }
  
  # Define common theme
  common_theme <- theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 14)),
      axis.title.x = element_text(size = 12, margin = margin(t = 12)),
      axis.title.y = element_text(size = 12, margin = margin(r = 12)),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.title = element_text(size = 12, margin = margin(b = 12)),
      legend.text = element_text(size = 12),
      legend.key.size = unit(1, "cm")
    )
  
  # Pre-compute color palettes
  max_abs_V5 <- max(abs(results$V5), na.rm = TRUE)
  CV <- ceiling(max_abs_V5 / (10 ^ floor(log10(max_abs_V5)))) * (10 ^ floor(log10(max_abs_V5)))
  
  color_palettes <- list(
    avg = c("blue4", "blue", "cyan", "green", "yellow", "red", "red3"),
    trend = {
      neg <- colorRampPalette(c("blue4", "blue", "cyan"))(33)
      pos <- colorRampPalette(c("yellow", "red", "red4"))(33)
      c(neg, "white", pos)
    },
    pval = {
      neg_p <- colorRampPalette(c("blue4", "blue", "cyan", "green", "yellow", "red", "red3", "magenta"))(32)
      pos_p <- colorRampPalette(c("grey34", "grey44", "grey54", "grey64", "grey74", "grey84", "grey92", "grey98"))(33)
      c(rev(neg_p), pos_p)
    }
  )
  
  # Create plot function
  create_subplot <- function(data, x, y, color_var, title, color_title, palette, limits = NULL) {
    ggplot(data, aes(x = {{x}}, y = {{y}}, color = {{color_var}})) +
      geom_point(size = 0.9 * scale, stroke = 0.3 * scale) +
      scale_shape_manual(values = c("1" = 19, "2" = 21), guide = "none") +
      scale_color_gradientn(colors = palette, limits = limits) +
      labs(title = title, x = "Central Time", y = "Time Window", color = color_title) +
      common_theme +
      coord_cartesian(xlim = c(0, n), ylim = c(0, n))
  }
  
  # Generate plots
  plot1 <- create_subplot(results, V1, V2, V3, 
                          "MOVING AVERAGE ANALYSIS\nAverages of all possible sub-series", 
                          "Average", color_palettes$avg)
  
  plot2 <- create_subplot(results, V1, V2, V5, 
                          "MOVING TREND ANALYSIS\nTrends of all possible sub-series", 
                          "Trend", color_palettes$trend, c(-CV, CV))
  
  plot3 <- create_subplot(subset(results, V4 <= 0.2), V1, V2, V4,
                          "MOVING AVERAGE ANALYSIS\nP-values of T-tests on averages",
                          "P-Value", color_palettes$pval, c(0, 0.2))
  
  plot4 <- create_subplot(subset(results, V6 <= 0.2), V1, V2, V6,
                          "MOVING TREND ANALYSIS\nP-values of Kendall's trend tests",
                          "P-Value", color_palettes$pval, c(0, 0.2))
  
  # Combine plots
  combined_plot <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
  
  # Save to file if requested
  if (!is.null(output_file)) {
    ggsave(output_file, combined_plot, width = 50, height = 42, 
           units = "cm", dpi = 600, bg = "white")
  }
  
  return(combined_plot)
}