library(ggplot2)
library(gridExtra)
library(Kendall)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Caricamento dei dati dal file Serie.txt
serie <- scan("SerieR.txt")
n <- length(serie)

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

# Media totale della serie
media_totale <- mean(serie)

# Loop per popolare la matrice
for (ind in 1:LEN_MAX) {
    centro <- x_values[ind]
    len <- y_values[ind]
    start <- max(1, round(centro - (len - 1) / 2))
    end <- min(n, ceiling(centro + (len - 1) / 2))
    
    # Estrazione della sottoserie
    sottoserie <- serie[start:end]
    
    if (length(sottoserie) < 6) next  # Continua solo se la lunghezza è >= 6
    
    # Calcolo del valore medio della sottoserie
    media_sottoserie <- mean(sottoserie)
    
    # Calcolo del t-test rispetto alla media totale
    t_test <- tryCatch(t.test(sottoserie, mu = media_totale)$p.value, error = function(e) NA)
    
    # Calcolo del trend della sottoserie (coefficiente di regressione lineare)
    trend <- tryCatch(lm(sottoserie ~ seq_along(sottoserie))$coefficients[2], error = function(e) NA)
    
    # Calcolo del test di Mann-Kendall sul trend della sottoserie
    mann_kendall <- tryCatch(MannKendall(sottoserie)$sl, error = function(e) NA)
    
    # Trasformazione della matrice in un dataframe con nomi delle colonne
    results[ind, 1] <- centro
    results[ind, 2] <- len
    results[ind, 3] <- media_sottoserie
    results[ind, 4] <- t_test
    results[ind, 5] <- trend
    results[ind, 6] <- mann_kendall
  }

results <- as.data.frame(results)

create_plots <- function(results) {

plot1 <- ggplot(results, aes(x = V1, y = V2, color = V3)) +  # Assumendo che le colonne si chiamino V1, V2, V3
  geom_point(size = 2) +  # Aggiungi i punti
  scale_color_gradientn(colors = c("blue4", "blue", "cyan", "green", "yellow", "red", "red3")) +  # Gradienti di colore
  #geom_point(data = subset(results, V4 > 0.1), aes(), size = 1, color = "white", alpha = 0.6) +
  #geom_point(data = subset(results, V4 > 0.1), aes(), shape = 4, size = 1, color = "magenta", alpha = 0.6) +
  labs(title = "Grafico a dispersione con colore basato su 'Valore di Z'",
       x = "TIMEWINDOW (Time Units)",
       y = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "Average") +  # Etichette
  theme_minimal()  # Tema minimalista
  plot1 <- plot1 + ggtitle("MOVING AVERAGE ANALYSIS \n Averages of all possible sub-series") +
  theme(plot.title = element_text(hjust = 0.5))
  
CV = ceiling(max(abs(results$V5)))
colors_neg <- colorRampPalette(c("blue4","blue","cyan"))(33)
colors_pos <- colorRampPalette(c("yellow","red","red4"))(33)

# Combina le scale colore con un contrasto netto attorno a 0
combined_colors <- c(colors_neg,"white",colors_pos)

plot2 <- ggplot(results, aes(x = V1, y = V2, color = V5)) +  # Assumendo che le colonne si chiamino V1, V2, V3
  geom_point(size = 2) +  # Aggiungi i punti
  scale_color_gradientn(
    colors = combined_colors,
    limits = c(-CV, CV)
  ) +
  #geom_point(data = subset(results, V6 > 0.1), aes(), size = 1, color = "white", alpha = 0.6) +
  labs(title = "Grafico a dispersione con colore basato su 'Valore di Z'",
       x = "TIMEWINDOW (Time Units)",
       y = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
       color = "Average") +  # Etichette
  theme_minimal()  # Tema minimalista
  plot2 <- plot2 + ggtitle("MOVING TREND ANALYSIS \n Trends of all possible sub-series") +
  theme(plot.title = element_text(hjust = 0.5))
  
  colors_neg_p <- colorRampPalette(c("blue4", "blue", "cyan", "green", "yellow", "red", "red3", "magenta"))(32)
  colors_pos_p <- colorRampPalette(c("grey34","grey44","grey54", "grey64", "grey74", "grey84", "grey92", "grey98"))(33)
  
  # Combina le scale colore con un contrasto netto attorno a 0
  combined_colors_p <- c(rev(colors_neg_p), colors_pos_p)
  
  plot3 <- ggplot(subset(results, V4 <= 0.2), aes(x = V1, y = V2, color = V4)) +  # Assumendo che le colonne si chiamino V1, V2, V3
    geom_point(size = 2) +  # Aggiungi i punti
    scale_color_gradientn(
      colors = combined_colors_p,
      limits = c(0, 0.2)
    ) +
    labs(title = "Grafico a dispersione con colore basato su 'Valore di Z'",
         x = "TIMEWINDOW (Time Units)",
         y = "CENTRAL TIME OF THE TIME WINDOW (Time Mark)",
         color = "Average") +  # Etichette
    theme_minimal()  # Tema minimalista
  plot3 <- plot3 + ggtitle("MOVING TREND ANALYSIS \n Trends of all possible sub-series") +
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
         color = "Average") +  # Etichette
    theme_minimal()  # Tema minimalista
  plot4 <- plot4 + ggtitle("MOVING TREND ANALYSIS \n Trends of all possible sub-series") +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
  
  ggsave("temp_filename.png", plot, width = 50, height = 42, units = "cm", dpi = 600, bg = "white")
}

create_plots(results)