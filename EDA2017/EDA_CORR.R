library(ggplot2)
library(gridExtra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

start.time <- Sys.time()

# Load external functions
source("kendall_tau.R")

# Data upload from SerieX.txt
serie <- scan("Serie1.txt")
serie2 <- scan("Serie1b.txt")
n <- length(serie)
n2 <- length(serie2)

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
results <- matrix(NA, ncol = 8, nrow = LEN_MAX)

# Loop to populate the matrix
for (ind in 1:LEN_MAX) {
    centro <- x_values[ind]
    len <- y_values[ind]
    start <- max(1, ceiling(centro - (len - 1) / 2))
    end <- min(n, ceiling(centro + (len - 1) / 2))
    
    # Sub-series extraction
    sottoserie <- serie[start:end]
    sottoserie2 <- serie2[start:end]
    
    if (length(sottoserie) < s) next
    
    # Rimuovo gli NA in entrambe le sottoserie, rimuovendo i corrispondenti nelle due serie
    validi_indices <- complete.cases(sottoserie, sottoserie2)
    
    # Filtra le sottoserie
    sottoserie <- sottoserie[validi_indices]
    sottoserie2 <- sottoserie2[validi_indices]

    # Populate and convert the matrix in a dataframe
    # Calcolare la correlazione Pearson e Kendall con tryCatch
    pearson <- tryCatch({
      cor.test(sottoserie, sottoserie2, method = "pearson", use = "complete.obs")
    }, error = function(e) NA)

    kendall <- tryCatch({
      #cor.test(sottoserie, sottoserie2, method = "kendall", use = "complete.obs")$p.value
      kendall_tau(sottoserie, sottoserie2, alternative = c("greater"))
    }, error = function(e) NA)
    
    pearson_cor <- pearson$estimate
    pearson_pvalue <- pearson$p.value
    kendall_cor <- kendall$tau
    kendall_pvalue <- kendall$p_value
    
    # Popolare la matrice con i risultati
    results[ind, 1] <- centro
    results[ind, 2] <- len
    results[ind, 3] <- pearson_cor
    results[ind, 4] <- pearson_pvalue
    results[ind, 5] <- kendall_cor
    results[ind, 6] <- kendall_pvalue
    results[ind, 7] <- ifelse(pearson_pvalue <= 0.1, 1, 2)  # MASKING
    results[ind, 8] <- ifelse(kendall_pvalue <= 0.1, 1, 2)  # MASKING
}

results <- as.data.frame(results)

create_plots <- function(results) {

CV = ceiling(max(abs(results$V5)))
colors_neg <- colorRampPalette(c("blue4","blue","cyan"))(33)
colors_pos <- colorRampPalette(c("yellow","red","red4"))(33)
combined_colors <- c(colors_neg,"white",colors_pos)

colors_neg_p <- colorRampPalette(c("blue4", "blue", "cyan", "green", "yellow", "red", "red3", "magenta"))(32)
colors_pos_p <- colorRampPalette(c("grey34","grey44","grey54", "grey64", "grey74", "grey84", "grey92", "grey98"))(33)
combined_colors_p <- c(rev(colors_neg_p), colors_pos_p)

### PEARSON TEST ###
plot <- ggplot(results, aes(x = V1, y = V2, color = V3, shape = factor(V7))) + 
  geom_point(size = 0.9*scale, stroke = 0.3*scale) +
  scale_shape_manual(
    values = c("1" = 19, "2" = 21),
    guide = "none"
  ) +
  scale_color_gradientn(
    colors = combined_colors,
    limits = c(-1, 1)
  ) +
  labs(title = "Pearson Correlation",
       y = "TIMEWINDOW (Time Units)",
       x = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "Pearson") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 14)),
    axis.title.x = element_text(size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(size = 12, margin = margin(r = 12)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12, margin = margin(b = 12)),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    #panel.grid.major = element_line(color = "black", size = 0.5),
    #panel.grid.minor = element_line(color = "gray67", size = 0.3),
    #panel.grid.minor.major = element_line(color = "gray", size = 0.2)
  ) +
  xlim(0, n) +
  ylim(0, n)
plot1 <- plot + ggtitle("CORRELATION TESTS \n Pearson correlation coefficient of all possible sub-series") +
  theme(plot.title = element_text(hjust = 0.5))

plot2 <- ggplot(subset(results, V4 <= 0.2), aes(x = V1, y = V2, color = V4)) +
  geom_point(size = 0.9*scale, stroke = 0.3*scale, shape = 19) +
  scale_color_gradientn(
    colors = combined_colors_p,
    limits = c(0, 0.2)
  ) +
  labs(title = "Pearson p-Value",
       y = "TIMEWINDOW (Time Units)",
       x = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "p-Value") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 14)),
    axis.title.x = element_text(size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(size = 12, margin = margin(r = 12)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12, margin = margin(b = 12)),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    #panel.grid.major = element_line(color = "black", size = 0.5),
    #panel.grid.minor = element_line(color = "gray67", size = 0.3),
    #panel.grid.minor.major = element_line(color = "gray", size = 0.2)
  ) +
  xlim(0, n) +
  ylim(0, n)
plot2 <- plot2 + ggtitle("CORRELATION TESTS \n Pearson correlation p-values for all possible sub-series") +
  theme(plot.title = element_text(hjust = 0.5))

### KENDALL TEST ###
plot3 <- ggplot(results, aes(x = V1, y = V2, color = V5, shape = factor(V8))) +
  geom_point(size = 0.9*scale, stroke = 0.3*scale) +
  scale_shape_manual(
    values = c("1" = 19, "2" = 21),
    guide = "none"
  ) +
  scale_color_gradientn(
    colors = combined_colors,
    limits = c(-1, 1)
  ) +
  labs(title = "Kendall Correlation",
       y = "TIMEWINDOW (Time Units)",
       x = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "Kendall") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 14)),
    axis.title.x = element_text(size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(size = 12, margin = margin(r = 12)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12, margin = margin(b = 12)),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    #panel.grid.major = element_line(color = "black", size = 0.5),
    #panel.grid.minor = element_line(color = "gray67", size = 0.3),
    #panel.grid.minor.major = element_line(color = "gray", size = 0.2)
  ) +
  xlim(0, n) +
  ylim(0, n)
plot3 <- plot3 + ggtitle("CORRELATION TESTS \n Kendall correlation coefficient of all possible sub-series") +
  theme(plot.title = element_text(hjust = 0.5))

plot4 <- ggplot(subset(results, V6 <= 0.2), aes(x = V1, y = V2, color = V6)) +
  geom_point(size = 0.9*scale, stroke = 0.3*scale, shape = 19) +
  scale_color_gradientn(
    colors = combined_colors_p,
    limits = c(0, 0.2)
  ) +
  labs(title = "Kendall p-Value",
       y = "TIMEWINDOW (Time Units)",
       x = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "p-Value") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 14)),
    axis.title.x = element_text(size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(size = 12, margin = margin(r = 12)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12, margin = margin(b = 12)),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    #panel.grid.major = element_line(color = "black", size = 0.5),
    #panel.grid.minor = element_line(color = "gray67", size = 0.3),
    #panel.grid.minor.major = element_line(color = "gray", size = 0.2)
  ) +
  xlim(0, n) +
  ylim(0, n)
plot4 <- plot4 + ggtitle("CORRELATION TESTS \n Kendall correlation p-values for all possible sub-series") +
  theme(plot.title = element_text(hjust = 0.5))

plot <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
  
ggsave("temp_filename.png", plot, width = 50, height = 42, units = "cm", dpi = 600, bg = "white")
}

create_plots(results)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)