library(ggplot2)
library(gridExtra)
library(Kendall)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data upload from Serie.txt
serie <- scan("SerieT.txt")
n <- length(serie)

# Creation of lists for x_values and y_values
x_values <- c()
y_values <- c()
m = 1 # 1 = NO SUBSAMPLE; > 1 Scale MUST BE AN INTEGER > 1
s = 6

# SAFEGUARD FOR VERY LONG SERIES
if (n > 250){
  m = ceiling(n/200)
  s = max(s, m)
}

scale <- 200 / n * m

# Generation of x_values and y_values
for (len in seq(s, n, by = m)) {
  
  for (y in seq(len/2, n-len/2, by = m)) {
    centro <- y
    x_values <- c(x_values, centro)
    y_values <- c(y_values, len)}
}

LEN_MAX <- length(x_values)

# Creation of an empty matrix to store the results
results <- matrix(NA, ncol = 7, nrow = LEN_MAX)

# Global average of the serie.
validi_indices <- complete.cases(serie)
serie_cleaned <- serie[validi_indices]
media_totale <- mean(serie_cleaned)

# Loop to populate the matrix
for (ind in 1:LEN_MAX) {
    centro <- x_values[ind]
    len <- y_values[ind]
    start <- max(1, round(centro - (len - 1) / 2))
    end <- min(n, ceiling(centro + (len - 1) / 2))
    
    # Sub-series extraction
    sottoserie <- serie[start:end]
    
    if (length(sottoserie) < s) next
    
    sottoserie <- sottoserie[is.finite(sottoserie) & complete.cases(sottoserie)]
    
    if (length(unique(sottoserie)) < 4) next
    
    # Values
    mean_subserie <- mean(sottoserie)
    t_test <- tryCatch(t.test(sottoserie, mu = media_totale)$p.value, error = function(e) NA)
    trend <- tryCatch(lm(sottoserie ~ seq_along(sottoserie))$coefficients[2], error = function(e) NA)
    mann_kendall <- tryCatch(MannKendall(sottoserie)$sl, error = function(e) NA)
    
    # Convert the matrix in a dataframe
    results[ind, 1] <- centro
    results[ind, 2] <- len
    results[ind, 3] <- mean_subserie
    results[ind, 4] <- t_test
    results[ind, 5] <- trend
    results[ind, 6] <- mann_kendall
    results[, 7] <- ifelse(results[, 4] <= 0.1, 1, 2) #MASKING
  }

results <- as.data.frame(results)

create_plots <- function(results) {

### AVERAGES ###
plot1 <- ggplot(results, aes(x = V1, y = V2, color = V3)) +
  geom_point(size = 0.9*scale, stroke = 0.3*scale) +
  scale_shape_manual(
    values = c("1" = 19, "2" = 21),
    guide = "none"
  ) +
  scale_color_gradientn(colors = c("blue4", "blue", "cyan", "green", "yellow", "red", "red3")) +
  labs(title = "AVERAGES",
       y = "TIMEWINDOW (Time Units)",
       x = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "Average") +
  theme_minimal() +
  xlim(0, n) +
  ylim(0, n)
  plot1 <- plot1 + ggtitle("MOVING AVERAGE ANALYSIS \n Averages of all possible sub-series") +
  theme(plot.title = element_text(hjust = 0.5))
  
max_abs_V5 <- abs(results$V5)
max_abs_V5 <- max(max_abs_V5, na.rm = TRUE)
CV <- 10^round(log10(max_abs_V5))
CV <- ceiling(max_abs_V5/CV)*CV
colors_neg <- colorRampPalette(c("blue4","blue","cyan"))(33)
colors_pos <- colorRampPalette(c("yellow","red","red4"))(33)
combined_colors <- c(colors_neg,"white",colors_pos)

### TRENDS ###
plot2 <- ggplot(results, aes(x = V1, y = V2, color = V5)) +
  geom_point(size = 0.9*scale, stroke = 0.3*scale) +
  scale_shape_manual(
    values = c("1" = 19, "2" = 21),
    guide = "none"
  ) +
  scale_color_gradientn(
    colors = combined_colors,
    limits = c(-CV, CV)
  ) +
  labs(title = "TRENDS",
       y = "TIMEWINDOW (Time Units)",
       x = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "Trends") +
  theme_minimal() +
  xlim(0, n) +
  ylim(0, n)
  plot2 <- plot2 + ggtitle("MOVING TREND ANALYSIS \n Trends of all possible sub-series") +
  theme(plot.title = element_text(hjust = 0.5))
  
colors_neg_p <- colorRampPalette(c("blue4", "blue", "cyan", "green", "yellow", "red", "red3", "magenta"))(32)
colors_pos_p <- colorRampPalette(c("grey34","grey44","grey54", "grey64", "grey74", "grey84", "grey92", "grey98"))(33)
combined_colors_p <- c(rev(colors_neg_p), colors_pos_p)

### P-VALUES AVERAGES ###  
plot3 <- ggplot(subset(results, V4 <= 0.2), aes(x = V1, y = V2, color = V4)) +
  geom_point(size = 0.9*scale, stroke = 0.3*scale) +
  scale_shape_manual(
    values = c("1" = 19, "2" = 21),
    guide = "none"
  ) +
  scale_color_gradientn(
    colors = combined_colors_p,
    limits = c(0, 0.2)
  ) +
  labs(title = "P-VALUES ON AVERAGES",
       y = "TIMEWINDOW (Time Units)",
       x = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "P-Value") +
  theme_minimal() +
  xlim(0, n) +
  ylim(0, n)
plot3 <- plot3 + ggtitle("MOVING AVERAGE ANALYSIS \n P-values of T-tests on averages") +
  theme(plot.title = element_text(hjust = 0.5))

### P-VALUES TREND ###
plot4 <- ggplot(subset(results, V6 <= 0.2), aes(x = V1, y = V2, color = V6)) +
  geom_point(size = 0.9*scale, stroke = 0.3*scale) +
  scale_shape_manual(
    values = c("1" = 19, "2" = 21),
    guide = "none"
  ) +
  scale_color_gradientn(
    colors = combined_colors_p,
    limits = c(0, 0.2)
  ) +
  labs(title = "P-VALUES ON TRENDS",
       y = "TIMEWINDOW (Time Units)",
       x = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "P-Values") +
  theme_minimal() +
  xlim(0, n) +
  ylim(0, n)
plot4 <- plot4 + ggtitle("MOVING TREND ANALYSIS \n P-values of kendall's trend tests") +
  theme(plot.title = element_text(hjust = 0.5))
  
### PLOT SAVE ###
plot <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
  
  ggsave("temp_filename.png", plot, width = 50, height = 42, units = "cm", dpi = 600, bg = "white")
}

create_plots(results)