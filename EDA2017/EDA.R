library(ggplot2)
library(gridExtra)

# Set working directory to the script's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Start time tracking
start.time <- Sys.time()

# Load external functions
source("mann_kendall.R")

# Data upload from SerieX.txt
serie <- scan("SerieSW.txt")
n <- length(serie)

# Initialize parameters
m <- if (n > 250) ceiling(n / 200) else 1  # Safeguard for long series
s <- 6 * m  # Scale parameter

scale <- 200 / n * m

# Generate x and y value sequences
seq_x <- seq(s, n, by = m)
seq_y <- seq(s / 2, n - s / 2, by = m)

# Pre-compute lengths to reduce repetitive calculations
len_x <- length(seq_x)
len_y <- length(seq_y)
LEN_MAX <- len_x * len_y

# Pre-allocate vectors and matrix for results
x_values <- numeric(LEN_MAX)
y_values <- numeric(LEN_MAX)
results <- matrix(NA, ncol = 7, nrow = LEN_MAX)

# Populate x_values and y_values
index <- 1
for (len in seq_x) {
  for (y in seq(len / 2, n - len / 2, by = m)) {
    x_values[index] <- y
    y_values[index] <- len
    index <- index + 1
  }
}

# Compute global mean of the series, filtering invalid values
serie_cleaned <- serie[is.finite(serie) & complete.cases(serie)]
media_totale <- mean(serie_cleaned)

# Function to process sub-series and populate the results matrix
process_subseries <- function(ind) {
  centro <- x_values[ind]
  len <- y_values[ind]
  start <- max(1, round(centro - (len - 1) / 2))
  end <- min(n, ceiling(centro + (len - 1) / 2))
  
  # Extract and clean sub-series
  sottoserie <- serie[start:end]
  sottoserie <- sottoserie[is.finite(sottoserie) & complete.cases(sottoserie)]
  
  # Skip small or repetitive sub-series
  if (length(sottoserie) < s || length(unique(sottoserie)) < 4) return(NULL)
  
  # Compute values
  mean_subserie <- mean(sottoserie)
  t_test <- tryCatch(t.test(sottoserie, mu = media_totale)$p.value, error = function(e) NA)
  trend <- tryCatch(lm(sottoserie ~ seq_along(sottoserie))$coefficients[2], error = function(e) NA)
  mk <- tryCatch(mann_kendall(sottoserie)$p.value, error = function(e) NA)
  
  # Return row for the results matrix
  c(centro, len, mean_subserie, t_test, trend, mk, ifelse(!is.na(t_test) && t_test <= 0.1, 1, 2))
}

# Parallelize computation for efficiency
library(parallel)
results_list <- mclapply(1:LEN_MAX, process_subseries, mc.cores = detectCores() - 1)
results <- do.call(rbind, results_list)

# Convert results to a data frame
results <- as.data.frame(results)
colnames(results) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7")

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

# End time tracking
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)