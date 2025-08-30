#' Plot hierarchical moving-window statistical analysis for time series.
#' @description
#' Plots hierarchical moving-window statistical analysis for custom functions. 
#' The input is a `data.frame` created by TH_tweak() function.
#'
#' @param `results` is a `data.frame` created by TH_tweak().
#' @param `scale` a `numeric`, scaling factor for point sizes, autoset by default.
#' @param `output_file` is a `string`, optional, filename to save plot (NULL to skip saving).
#' @param `colorscales` a `list` of color scales for each function (optional). Can mix predefined names ("avg", "trend", "pval") and custom color vectors.
#' @param `colorlimits` a `list` of numeric vectors specifying color limits for each function (optional). Each element should be a vector of length 2 (c(min, max)). If NULL, limits are calculated automatically.
#' @return A `gridExtra` arranged plot object.
#' @export
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices colorRampPalette
#' @author Vladimiro Andrea Boselli, (2025) \email{boselli.v@@irea.cnr.it}
#' @examples
#'  \dontrun{
#' results <- TH_tweak(fun1, fun2, fun3, series1, series2)
#' TH_plott(results)
#' TH_plott(results, colorscales = list(c("blue", "white", "red"), "avg", "trend"))
#' }
### function TH_plott
TH_plott <- function(results, scale = NULL, output_file = NULL, colorscales = NULL, colorlimits = NULL) {
  
  # Extract attributes
  n <- attr(results, "n")
  m <- attr(results, "m")
  s <- attr(results, "s")
  alpha <- attr(results, "param")
  fun_names <- attr(results, "functions")
  
  # Calculate scale if not provided
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
  
  # Helper function to get predefined palettes
  get_palette <- function(pal_name) {
    palettes <- list(
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
    if (pal_name %in% names(palettes)) palettes[[pal_name]] else pal_name
  }
  
  # Get function result columns
  fun_cols <- grep("^F\\d+$", names(results), value = TRUE)
  n_funs <- length(fun_cols)
  
  # Generate plots for each function
  plot_list <- lapply(seq_along(fun_cols), function(i) {
    col_name <- fun_cols[i]
    values <- results[[col_name]]
    
    # Determine color palette based on values or user input
    if (!is.null(colorscales) && length(colorscales) >= i) {
      scale_candidate <- colorscales[[i]]
      
      # Handle predefined palette names
      if (is.character(scale_candidate) && length(scale_candidate) == 1) {
        color_scale <- get_palette(scale_candidate)
      } 
      # Handle custom color vectors
      else {
        color_scale <- scale_candidate
      }
    } 
    # Auto-select color scale if not provided
    else {
      if (all(values >= 0, na.rm = TRUE) || all(values <= 0, na.rm = TRUE)) {
        color_scale <- get_palette("avg")
      } else {
        color_scale <- get_palette("trend")
      }
    }
    
    # Determine color limits
    if (!is.null(colorlimits) && length(colorlimits) >= i) {
      limits_candidate <- colorlimits[[i]]
      if (is.numeric(limits_candidate) && length(limits_candidate) == 2) {
        limits <- limits_candidate
      } else {
        # Fallback to automatic calculation if user-provided limits are invalid
        if (all(values >= 0, na.rm = TRUE)) {
          limits <- c(0, max(values, na.rm = TRUE))
        } else if (all(values <= 0, na.rm = TRUE)) {
          limits <- c(min(values, na.rm = TRUE), 0)
        } else {
          max_abs <- max(abs(range(values, na.rm = TRUE)))
          limits <- c(-max_abs, max_abs)
        }
      }
    } else {
      # Automatic calculation if no user-provided limits
      if (all(values >= 0, na.rm = TRUE)) {
        limits <- c(0, max(values, na.rm = TRUE))
      } else if (all(values <= 0, na.rm = TRUE)) {
        limits <- c(min(values, na.rm = TRUE), 0)
      } else {
        max_abs <- max(abs(range(values, na.rm = TRUE)))
        limits <- c(-max_abs, max_abs)
      }
    }
    
    # Create plot title
    if (!is.null(fun_names) && length(fun_names) >= i) {
      title <- paste("FUNCTION ANALYSIS:", fun_names[i])
    } else {
      title <- paste("FUNCTION ANALYSIS:", col_name)
    }
    
    # Filter out NA values
    data_plot <- results[!is.na(results[[col_name]]), ]
    
    # Create the plot
    ggplot(data_plot, aes(x = center, y = length, color = .data[[col_name]])) +
      geom_point(size = scale, stroke = 0.2 * scale) +
      scale_color_gradientn(
        colors = color_scale,
        limits = limits
      ) +
      labs(
        title = title,
        x = "Central Time", 
        y = "Time Window", 
        color = "Value"
      ) +
      common_theme +
      coord_cartesian(xlim = c(0, n), ylim = c(0, n))
  })
  
  # Arrange plots based on number
  n_plots <- length(plot_list)
  if (n_plots == 0) {
    stop("No plots generated. Check if results contain function columns (F1, F2, ...)")
  }
  
  # Determine grid arrangement
  n_row <- round(sqrt(n_plots))
  n_col <- ceiling(n_plots / n_row)
  
  combined_plot <- grid.arrange(
    grobs = plot_list,
    ncol = n_col,
    nrow = n_row
  )
  
  # Save to file if requested
  if (!is.null(output_file)) {
    # Calculate dimensions based on number of panels
    base_width_per_panel <- 25  # cm per panel
    base_height_per_panel <- 21 # cm per panel
    
    width <- base_width_per_panel * n_col
    height <- base_height_per_panel * n_row
    
    ggsave(
      output_file, combined_plot,
      width = width,
      height = height,
      units = "cm", dpi = 600, bg = "white"
    )
  }
  
  return(combined_plot)
}