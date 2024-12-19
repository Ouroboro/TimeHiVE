library(ggplot2)
library(gridExtra)
library(Kendall)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Caricamento dei dati dal file Serie.txt
serie <- scan("SerieR.txt")
serie2 <- scan("SerieT.txt")
n <- length(serie)
n2 <- length(serie2)

# Inizializzazione delle liste per x_values e y_values
x_values <- c()
y_values <- c()

# Generazione dei valori per x_values e y_values
for (len in 6:n) {
  for (y in 1:(n-len+1)) {
    centro <- len/2+y-1
    x_values <- c(x_values, centro)
    y_values <- c(y_values, len)
  }
}

LEN_MAX <- length(x_values)

# Creazione di una matrice vuota per salvare i risultati
results <- matrix(NA, ncol = 6, nrow = LEN_MAX)

# Loop per popolare la matrice
for (ind in 1:LEN_MAX) {
    centro <- x_values[ind]
    len <- y_values[ind]
    start <- max(1, round(centro - (len - 1) / 2))
    end <- min(n, ceiling(centro + (len - 1) / 2))
    
    # Estrazione della sottoserie
    sottoserie <- serie[start:end]
    sottoserie2 <- serie2[start:end]
    
    if (length(sottoserie) < 6) next  # Continua solo se la lunghezza è >= 6
    # if (length(sottoserie2) < 6) next

    # Trasformazione della matrice in un dataframe con nomi delle colonne
    results[ind, 1] <- centro
    results[ind, 2] <- len
    results[ind, 3] <- cor.test(sottoserie, sottoserie2, method = "pearson")$estimate
    results[ind, 4] <- cor.test(sottoserie, sottoserie2, method = "pearson")$p.value
    results[ind, 5] <- cor.test(sottoserie, sottoserie2, method = "kendall")$estimate
    results[ind, 6] <- cor.test(sottoserie, sottoserie2, method = "kendall")$p.value
  }

results <- as.data.frame(results)

create_plots <- function(results) {

CV = ceiling(max(abs(results$V5)))
colors_neg <- colorRampPalette(c("blue4","blue","cyan"))(33)
colors_pos <- colorRampPalette(c("yellow","red","red4"))(33)
combined_colors <- c(colors_neg,colors_pos)

colors_neg_p <- colorRampPalette(c("blue4", "blue", "cyan", "green", "yellow", "red", "red3", "magenta"))(32)
colors_pos_p <- colorRampPalette(c("grey34","grey44","grey54", "grey64", "grey74", "grey84", "grey92", "grey98"))(33)
combined_colors_p <- c(rev(colors_neg_p), colors_pos_p)

plot1 <- ggplot(results, aes(x = V1, y = V2, color = V3)) +  # Assumendo che le colonne si chiamino V1, V2, V3
  geom_point(size = 2) +  # Aggiungi i punti
  scale_color_gradientn(
    colors = combined_colors,
    limits = c(-1, 1)
  ) +  # Gradienti di colore
  #geom_point(data = subset(results, V4 > 0.1), aes(), size = 1, color = "white", alpha = 0.6) +
  #geom_point(data = subset(results, V4 > 0.1), aes(), shape = 4, size = 1, color = "magenta", alpha = 0.6) +
  labs(title = "Grafico a dispersione con colore basato su 'Valore di Z'",
       x = "TIMEWINDOW (Time Units)",
       y = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "Pearson") +  # Etichette
  theme_minimal()  # Tema minimalista
plot1 <- plot1 + ggtitle("CORRELATION TESTS \n Correlations of all possible sub-series") +
  theme(plot.title = element_text(hjust = 0.5))

plot2 <- ggplot(subset(results, V4 <= 0.2), aes(x = V1, y = V2, color = V4)) +  # Assumendo che le colonne si chiamino V1, V2, V3
  geom_point(size = 2) +  # Aggiungi i punti
  scale_color_gradientn(
    colors = combined_colors_p,
    limits = c(0, 0.2)
  ) +
  labs(title = "Grafico a dispersione con colore basato su 'Valore di Z'",
       x = "TIMEWINDOW (Time Units)",
       y = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "p-Value") +  # Etichette
  theme_minimal()  # Tema minimalista
plot2 <- plot2 + ggtitle("CORRELATION TESTS \n Correlations of all possible sub-series") +
  theme(plot.title = element_text(hjust = 0.5))
  
plot3 <- ggplot(results, aes(x = V1, y = V2, color = V5)) +  # Assumendo che le colonne si chiamino V1, V2, V3
  geom_point(size = 2) +  # Aggiungi i punti
  scale_color_gradientn(
    colors = combined_colors,
    limits = c(-1, 1)
  ) +  # Gradienti di colore
  #geom_point(data = subset(results, V4 > 0.1), aes(), size = 1, color = "white", alpha = 0.6) +
  #geom_point(data = subset(results, V4 > 0.1), aes(), shape = 4, size = 1, color = "magenta", alpha = 0.6) +
  labs(title = "Grafico a dispersione con colore basato su 'Valore di Z'",
       x = "TIMEWINDOW (Time Units)",
       y = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "Kendall") +  # Etichette
  theme_minimal()  # Tema minimalista
plot3 <- plot3 + ggtitle("CORRELATION TESTS \n Correlations of all possible sub-series") +
  theme(plot.title = element_text(hjust = 0.5))
  
plot4 <- ggplot(subset(results, V6 <= 0.2), aes(x = V1, y = V2, color = V6)) +  # Assumendo che le colonne si chiamino V1, V2, V3
  geom_point(size = 2) +  # Aggiungi i punti
  scale_color_gradientn(
    colors = combined_colors_p,
    limits = c(0, 0.2)
  ) +
  labs(title = "Grafico a dispersione con colore basato su 'Valore di Z'",
       x = "TIMEWINDOW (Time Units)",
       y = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "p-Value") +  # Etichette
  theme_minimal()  # Tema minimalista
plot4 <- plot4 + ggtitle("CORRELATION TESTS \n Correlations of all possible sub-series") +
  theme(plot.title = element_text(hjust = 0.5))
  
plot <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
  
ggsave("temp_filename.png", plot, width = 50, height = 42, units = "cm", dpi = 600, bg = "white")
}

create_plots(results)
