#' Plot hierarchical moving-window statistical analysis for time series.
#' @description
#' Plots hierarchical moving-window statistical analysis for single time
#' series. It returns analysis on moving average and running trends. The 
#' input is a `data.frame` created by TH_single() function.
#'
#' @param `result` is a `data.frame` created by to TH_single().
#' @param `scale` a `numeric`, scaling factor for point sizes, autoset by 
#' default.
#' @param `output_file` is a `string`, optional, filename to save plot (NULL to
#' skip saving), NULL by default.
#' @param `mask` a `logical` value. If TRUE, enhances main plots by using point 
#' shapes to indicate statistical significance (if available). Only applicable 
#' for modes: "all", "trend_with_test", and "avg_with_test". For other modes, 
#' a warning is issued and mask is ignored.
#' @param `mode` a `character` string specifying the computation mode. One of:
#' "all", "avg_only", "trend_only", "avg_trend", "trend_with_test", "avg_with_test".
#' By default it takes the mode passed as attribute by `results`.
#' @return A `ggplot` or `gridExtra` arranged plot object.
#' @export
#' @import ggplot2
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom grDevices colorRampPalette
#' @author Vladimiro Andrea Boselli, (2025) \email{boselli.v@@irea.cnr.it}
#' @examples
#'  \dontrun{
#' results <- TH_single(series, 4, 6)
#' TH_plots(results)
#' TH_plots(results, output_file = output_file, mask = TRUE, mode = "avg_with_test")
#' }
### function TH_plots
TH_plots <- function(results, scale = NULL, output_file = NULL, mask = FALSE, mode = NULL) {
  # Input validation
  if (!is.data.frame(results)) stop("results must be a data.frame")
  required_cols <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
  if (!all(required_cols %in% names(results))) {
    stop("results must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Extract attributes
  m <- attr(results, "m")
  s <- attr(results, "s")
  n <- attr(results, "n")
  alpha <- attr(results, "alpha")
  # Allow mode override
  if (is.null(mode)) {
    mode <- attr(results, "mode")
  } else {
    # Validate the new mode
    valid_modes <- c("all", "avg_only", "trend_only", "avg_trend", "trend_with_test", "avg_with_test")
    if (!mode %in% valid_modes) {
      stop("Invalid mode. Choose from: ", paste(valid_modes, collapse = ", "))
    }
  }
  
  
  if (is.null(scale)) scale <- 200 / n * m
  
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
  CV <- if (max_abs_V5 > 0) {
    ceiling(max_abs_V5 / (10 ^ floor(log10(max_abs_V5)))) * (10 ^ floor(log10(max_abs_V5)))
  } else {
    1  # Default value if no variation
  }
  
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
  
  # Define legend box function (fixed version)
  create_legend_box <- function(alpha) {
    list(
      annotate(
        "rect", 
        xmin = 0.02 * n, xmax = 0.25 * n, 
        ymin = 0.83 * n, ymax = 0.98 * n,
        fill = "white", color = "black", linewidth = 0.2, alpha = 1
      ),
      annotate(
        "text", x = 0.035 * n, y = 0.945 * n,
        label = "p-values filter:",
        hjust = 0, vjust = 0, size = 4, fontface = "bold"
      ),
      annotate(
        "point", x = rep(0.045 * n, 2), 
        y = c(0.905 * n, 0.865 * n),
        shape = c(19, 21), size = 4, 
        color = "black", fill = "white"
      ),
      annotate(
        "text", x = rep(0.065 * n, 2), 
        y = c(0.905 * n, 0.865 * n),
        label = c(
          paste0("p-value \u2264 ", alpha),
          paste0("p-value > ", alpha)
        ),
        hjust = 0, vjust = 0.5, size = 3.5
      )
    )
  }
  
  # Initialize plot list
  plot_list <- list()
  
  # Handle mask warnings for unsupported modes
  if (mask && !mode %in% c("all", "trend_with_test", "avg_with_test")) {
    warning("Mask parameter ignored for current mode. Only applicable to modes: all, trend_with_test, avg_with_test")
  }
  
  # Plot 1: Moving Average Analysis
  if (mode %in% c("all", "avg_only", "avg_trend", "avg_with_test")) {
    data_plot1 <- results[results$V1 != 0 & !is.na(results$V3), ]
    
    if (mask && mode %in% c("all", "avg_with_test")) {
      plot1 <- ggplot(data_plot1, aes(x = V1, y = V2, color = V3)) +
        geom_point(aes(shape = factor(V7)), size = scale, stroke = 0.2 * scale) +
        scale_color_gradientn(colors = color_palettes$avg) +
        scale_shape_manual(
          values = c(19, 21),
          guide = "none"
        ) +
        create_legend_box(alpha) +
        labs(
          title = "MOVING AVERAGE ANALYSIS\nAverages of all possible sub-series",
          x = "Central Time", y = "Time Window", color = "Average"
        ) +
        common_theme +
        coord_cartesian(xlim = c(0, n), ylim = c(0, n))
    } else {
      plot1 <- ggplot(data_plot1, aes(x = V1, y = V2, color = V3)) +
        geom_point(size = scale, stroke = 0.2 * scale) +
        scale_color_gradientn(colors = color_palettes$avg) +
        labs(
          title = "MOVING AVERAGE ANALYSIS\nAverages of all possible sub-series",
          x = "Central Time", y = "Time Window", color = "Average"
        ) +
        common_theme +
        coord_cartesian(xlim = c(0, n), ylim = c(0, n))
    }
    plot_list <- c(plot_list, list(plot1))
  }
  
  # Plot 2: Moving Trend Analysis
  if (mode %in% c("all", "trend_only", "avg_trend", "trend_with_test")) {
    data_plot2 <- results[!is.na(results$V5), ]
    
    if (mask && mode %in% c("all", "trend_with_test")) {
      plot2 <- ggplot(data_plot2, aes(x = V1, y = V2, color = V5)) +
        geom_point(aes(shape = factor(V8)), size = scale, stroke = 0.2 * scale) +
        scale_color_gradientn(colors = color_palettes$trend, limits = c(-CV, CV)) +
        scale_shape_manual(
          values = c(19, 21),
          guide = "none"
        ) +
        create_legend_box(alpha) +
        labs(
          title = "MOVING TREND ANALYSIS\nTrends of all possible sub-series",
          x = "Central Time", y = "Time Window", color = "Trend"
        ) +
        common_theme +
        coord_cartesian(xlim = c(0, n), ylim = c(0, n))
    } else {
      plot2 <- ggplot(data_plot2, aes(x = V1, y = V2, color = V5)) +
        geom_point(size = scale, stroke = 0.2 * scale) +
        scale_color_gradientn(colors = color_palettes$trend, limits = c(-CV, CV)) +
        labs(
          title = "MOVING TREND ANALYSIS\nTrends of all possible sub-series",
          x = "Central Time", y = "Time Window", color = "Trend"
        ) +
        common_theme +
        coord_cartesian(xlim = c(0, n), ylim = c(0, n))
    }
    plot_list <- c(plot_list, list(plot2))
  }
  
  # Plot 3: P-values of T-tests (only without mask)
  if (!mask && mode %in% c("all", "avg_with_test")) {
    data_plot3 <- subset(results, V4 <= 0.2 & !is.na(V4))
    plot3 <- ggplot(data_plot3, aes(x = V1, y = V2, color = V4)) +
      geom_point(size = scale, stroke = 0.2 * scale) +
      scale_color_gradientn(colors = color_palettes$pval, limits = c(0, 0.2)) +
      labs(
        title = "MOVING AVERAGE ANALYSIS\nP-values of T-tests on averages",
        x = "Central Time", y = "Time Window", color = "P-Value"
      ) +
      common_theme +
      coord_cartesian(xlim = c(0, n), ylim = c(0, n))
    plot_list <- c(plot_list, list(plot3))
  }
  
  # Plot 4: P-values of Kendall's tests (only without mask)
  if (!mask && mode %in% c("all", "trend_with_test")) {
    data_plot4 <- subset(results, V6 <= 0.2 & !is.na(V6))
    plot4 <- ggplot(data_plot4, aes(x = V1, y = V2, color = V6)) +
      geom_point(size = scale, stroke = 0.2 * scale) +
      scale_color_gradientn(colors = color_palettes$pval, limits = c(0, 0.2)) +
      labs(
        title = "MOVING TREND ANALYSIS\nP-values of Kendall's trend tests",
        x = "Central Time", y = "Time Window", color = "P-Value"
      ) +
      common_theme +
      coord_cartesian(xlim = c(0, n), ylim = c(0, n))
    plot_list <- c(plot_list, list(plot4))
  }
  
  # Arrange plots based on number
  n_plots <- length(plot_list)
  if (n_plots == 0) {
    stop("No plots generated for the given mode and mask settings")
  }
  
  if (n_plots == 1) {
    combined_plot <- plot_list[[1]]
  } else if (n_plots == 2) {
    combined_plot <- grid.arrange(grobs = plot_list, ncol = 2)
  # } else if (n_plots == 3) {
  #   # Arrange first two in top row, third in bottom row spanning both columns
  #   combined_plot <- grid.arrange(
  #     arrangeGrob(grobs = plot_list[1:2], ncol = 2),
  #     plot_list[[3]],
  #     nrow = 2,
  #     heights = c(1, 1)
  #   )
  } else {
    combined_plot <- grid.arrange(grobs = plot_list, ncol = 2, nrow = 2)
  }
  
  # Save to file if requested
  if (!is.null(output_file)) {
    ggsave(
      output_file, combined_plot,
      width = ifelse(n_plots > 1, 50, 25),
      height = ifelse(n_plots > 2, 42, 21),
      units = "cm", dpi = 600, bg = "white"
    )
  }
  
  return(combined_plot)
}