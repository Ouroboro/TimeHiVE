#' Create EDA Diagnostic Plots
#'
#' @param results Data.frame from eda_analyze()
#' @param scale Numeric, scaling factor for point sizes
#' @param output_file Optional, filename to save plot (NULL to skip saving)
#' @return A gridExtra arranged plot object
#' @export
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices colorRampPalette
eda_plot <- function(results, scale = NULL, output_file = NULL) {
  n <- max(results$V1, na.rm = TRUE)
  if (is.null(scale)) scale <- 200 / n * (mean(results$V2, na.rm = TRUE) / n
                                          
# Define function to create plots
create_plots <- function(results) {
  ### Define Color Scales ###
  max_abs_V5 <- max(abs(results$V5), na.rm = TRUE)
  CV <- ceiling(max_abs_V5 / (10 ^ floor(log10(max_abs_V5)))) * (10 ^ floor(log10(max_abs_V5)))
  
  colors_neg <- colorRampPalette(c("blue4", "blue", "cyan"))(33)
  colors_pos <- colorRampPalette(c("yellow", "red", "red4"))(33)
  combined_colors <- c(colors_neg, "white", colors_pos)
  
  colors_neg_p <- colorRampPalette(c("blue4", "blue", "cyan", "green", "yellow", "red", "red3", "magenta"))(32)
  colors_pos_p <- colorRampPalette(c("grey34", "grey44", "grey54", "grey64", "grey74", "grey84", "grey92", "grey98"))(33)
  combined_colors_p <- c(rev(colors_neg_p), colors_pos_p)
  
  ### Generate Plots ###
  plot1 <- ggplot(results, aes(x = V1, y = V2, color = V3)) +
    geom_point(size = 0.9 * scale, stroke = 0.3 * scale) +
    scale_color_gradientn(colors = c("blue4", "blue", "cyan", "green", "yellow", "red", "red3")) +
    labs(title = "Averages", x = "Central Time", y = "Time Window", color = "Average") +
    theme_minimal() +
    xlim(0, n) +
    ylim(0, n)
    theme(plot.title = element_text(hjust = 0.5))
  
  plot2 <- ggplot(results, aes(x = V1, y = V2, color = V5)) +
    geom_point(size = 0.9 * scale, stroke = 0.3 * scale) +
    scale_color_gradientn(colors = combined_colors, limits = c(-CV, CV)) +
    labs(title = "Trends", x = "Central Time", y = "Time Window", color = "Trend") +
    theme_minimal() +
    xlim(0, n) +
    ylim(0, n)
    theme(plot.title = element_text(hjust = 0.5))
  
  plot3 <- ggplot(subset(results, V4 <= 0.2), aes(x = V1, y = V2, color = V4)) +
    geom_point(size = 0.9 * scale, stroke = 0.3 * scale) +
    scale_color_gradientn(colors = combined_colors_p, limits = c(0, 0.2)) +
    labs(title = "P-Values (Averages)", x = "Central Time", y = "Time Window", color = "P-Value") +
    theme_minimal() +
    xlim(0, n) +
    ylim(0, n)
    theme(plot.title = element_text(hjust = 0.5))
  
  plot4 <- ggplot(subset(results, V6 <= 0.2), aes(x = V1, y = V2, color = V6)) +
    geom_point(size = 0.9 * scale, stroke = 0.3 * scale) +
    scale_color_gradientn(colors = combined_colors_p, limits = c(0, 0.2)) +
    labs(title = "P-Values (Trends)", x = "Central Time", y = "Time Window", color = "P-Value") +
    theme_minimal() +
    xlim(0, n) +
    ylim(0, n)
    theme(plot.title = element_text(hjust = 0.5))
  
  # Combine and save plots
  combined_plot <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
  ggsave("optimized_plots.png", combined_plot, width = 50, height = 42, units = "cm", dpi = 600, bg = "white")
}

# Create and save plots
create_plots(results)
                                        
  if (!is.null(output_file)) {
  ggsave(output_file, combined_plot, width = 50, height = 42, 
  units = "cm", dpi = 600, bg = "white")
  }
                                
  return(combined_plot)
}