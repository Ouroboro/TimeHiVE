library(ggplot2)
library(gifski)
library(data.table)

# Function to read data from a file and replace values less than -10 with NA
read_data <- function(filename) {
  data <- as.numeric(read.table(filename)$V1)
  data[data < -10] <- NA
  return(data)
}

# Function to compute same relative humidity lines
sh <- function(H, T) {
  pv <- (11.7 ^ (8.07131 - 1730.63 / (T + 233.426))) * 1.1
  mr <- 622 * pv / (1013.25 - pv)
  w <- mr * H / 100
  return(w)
}

# Function to compute isoenthalpic lines
wh <- function(Tl, h) {
  c_air <- 1006
  hlg <- 2501000
  cw <- 1860
  w <- (h * 1000 - c_air * Tl) / (hlg + cw * Tl)
  pv <- (11.7 ^ (8.07131 - 1730.63 / (Tl + 233.426))) * 1.1
  w_max <- 622 * pv / (1013.25 - pv)
  w[w * 1000 > w_max] <- NA
  Tl[w * 1000 > w_max] <- NA
  return (list(w, Tl))
}

# Function to compute mean parameters
seasonal_parameters <- function(T, H) {
  c_air <- 1.006
  hlg <- 2501
  cw <- 1.860
  pv <- (11.7 ^ (8.07131 - 1730.63 / (T + 233.426))) * 1.1
  mr <- 622 * pv / (1013.25 - pv)
  w <- mr * H / 100
  h <- c_air * T + (w / 1000) * (hlg + T * cw)
  return(c(mean(T, na.rm = TRUE), mean(H, na.rm = TRUE), mean(h, na.rm = TRUE), mean(w, na.rm = TRUE)))
}

# Function to create a psychrometric chart
psychroclimograph <- function(filenameT, filenameH) {
  
  # Data loading, stream data in here when you modify the code.
  T <- as.vector(read_data(filenameT))
  H <- as.vector(read_data(filenameH))

  # Calculate mean parameters
  params <- seasonal_parameters(T, H)
  Tx <- params[1]
  Hx <- params[2]
  hx <- params[3]
  wx <- params[4]
  
  # Creating occurrence table for value pairs (T, H)
  rounded_T <- round(T)
  paramB <- (min(na.omit(rounded_T)))
  print(paramB)
  ceil_sh <- ceiling(sh(H, T))
  
  # Combining T and sh(H, T) in a data frame and counting the occurrences
  v <- data.frame(rounded_T, ceil_sh)
  v <- na.omit(v)
  v <- v[v$rounded_T >= -10 & v$rounded_T <= 50 & v$ceil_sh >= 0 & v$ceil_sh <= 50, ]
  Mat <- as.data.frame(table(v))
  
  # Initialization of matrix M with NA values
  M <- matrix(NA, nrow = 61, ncol = 51)
  
  # Filling the matrix M with the counts from Mat
  for (i in 1:nrow(Mat)) {
    row <- Mat[i, ]
    x_index <- as.numeric(row$rounded_T)
    y_index <- as.numeric(row$ceil_sh)
    if (x_index >= -10 && x_index <= 50 && y_index >= 0 && y_index <= 50) {
      M[x_index, y_index] <- row$Freq
    }
  }
  
  # Matrix linearization and de-linearization
  M_linear <- as.vector(M)
  M <- matrix(M_linear, nrow = 61, ncol = 51, byrow = TRUE)
  
  # Creating long data frame for ggplot2
  long_M <- data.frame(expand.grid(x = -10:50, y = 0:50), count = as.vector(t(M)))
  
  # Set zero values to NA
  long_M$count[long_M$count == 0] <- NA

  # Function to create lines at constant relative humidity.
  create_line_data <- function(a) {
   Tl <- seq(-10, 50, by = 0.02)
   data.frame(
     x = Tl,
     y = sh(Hr, Tl)
   )
  }
  
  # Function to create lines at constant hentalpy.
  create_line_data2 <- function(h) {
    Tl2 <- seq(-10, 50, by = 0.02)
    c_air <- 1006
    wh_values <- wh(Tl2, h)
    w <- wh_values[[1]]
    Tl <- wh_values[[2]]
    
    finite_mask <- !is.na(Tl) & !is.na(w)
    data.frame(
      x = Tl[finite_mask],
      y = 1000 * w[finite_mask]
    )
  }

  # Values to use for drawing the psychrometric chart lines
  Hr_values <- seq(10, 100, by = 10)
  Hp_values <- seq(0, 180, by = 10)
  paramA <- (min(na.omit(ceil_sh)))
  print(paramA)
  
  # Plot creation
  plot <- ggplot() +
    geom_tile(data = long_M, aes(x = x+10.5+paramB, y = y-0.5+paramA, fill = count)) +
    scale_fill_gradientn(limits  = range(0, 300), colors = c("blue4","blue", "cyan","green", "yellow", "red","red3"), na.value = NA) +
    labs(x = "Temperature (°C)", y = "Specific Humidity (g vap/kg dry air)", fill = "Number of Measures") +
    scale_x_continuous(limits = c(-10, 50), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
    
    #TWEAKS
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA), # Set background to white
          plot.background = element_rect(fill = "white", color = NA), # Set plot background to white
          panel.grid.minor = element_blank()) + # Remove minor grid
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      legend.key.height = unit(3.5, "cm"),
      legend.key.width = unit(1, "cm"),
      legend.title = element_text(size = 10, angle = 90, hjust = 0.5),
      legend.text = element_text(size = 10), 
      legend.title.position = "right",
      axis.text.x = element_text(margin = margin(t = 2)),
      axis.text.y = element_text(margin = margin(r = 2)),
      panel.border = element_rect(fill = NA, color = "black", size = 1),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    guides(color = guide_colorbar(barwidth = 0.5, barheight = 1))+ # Set Colourbar
    coord_cartesian(xlim = c(-10, 50), ylim = c(0, 50), expand = FALSE) +
    scale_x_continuous(breaks = seq(-10, 50, by = 10))+
    annotate("text", x = 48.5, y = 7.7, label = "10% RH", size = 3, color = "gray64", angle = 23) +
    annotate("text", x = 48.5, y = 42.4, label = "50% RH", size = 3, color = "gray64", angle = 62) +
    annotate("text", x = 38.5, y = 46.5, label = "100% RH", size = 3, color = "gray64", angle = 63) +
    annotate("text", x = -5, y = 45, label = sprintf("AVERAGED VALUES \nTemperature %.2f °C \nRelative Humidity %.2f%% \nEnthalpy %.2f kJ/kg \nSpecific Humidity %.2f g/kg \nTotal Measures %d", Tx, Hx, hx, wx, sum(Mat$Freq, na.rm = TRUE)), color = "magenta", size = 4)
  print(Hx)
  plot <- plot +
    geom_point(aes(x = Tx, y = wx), color = "magenta", size = 3)
  
  # Draw same Relative Humidity lines
  for (Hr in Hr_values) {
    line_data <- create_line_data(Hr)
    plot <- plot + 
      geom_line(data = line_data, aes(x = x, y = y), color = "grey82", linewidth = 0.2)
  }
  
  # Draw Isohentalpic lines
  for (h in Hp_values) {
    line_data <- create_line_data2(h)
    plot <- plot + 
      geom_line(data = line_data, aes(x = x, y = y), linetype = "dotted", color = "grey82", linewidth = 0.4)

    i=1006*h/1000
    i <- sprintf("%.2f", i)

    max_y_point <- line_data[which.max(line_data$y), ]
    
    plot <- plot +
      geom_text(
        data = max_y_point,
        aes(x = x - 1.75, y = y + 0.75, angle = -15),
        label = paste(i, "(kJ/kg)"),
        vjust = -0.5,
        color = "gray24",
        size = 2.5
      )
  }
  
# Saving the graph as a temporary image
  temp_filename <- sprintf("temp_%s.png", substr(filenameT, nchar(filenameT)-7, nchar(filenameT)-4))
  plot <- plot + ggtitle(temp_filename) #TITLE
  ggsave(temp_filename, plot, width = 50, height = 21, units = "cm", dpi = 600, bg = "white")
  return(temp_filename)
}

# Function to create an animated GIF
animate <- function() {
  mypath <- getwd()
  images <- c()
  
  for (n in 1998:2015) {
    fileT <- file.path(mypath, "Dati_Arpa", sprintf("%d", n), sprintf("bsT%d.txt", n)) #Stream Data Here
    fileH <- file.path(mypath, "Dati_Arpa", sprintf("%d", n), sprintf("bsH%d.txt", n))
    temp_filename <- psychroclimograph(fileT, fileH)
    images <- c(images, temp_filename)
  }
  
  gif_file <- "station_name.gif"
  gifski(images, gif_file, width = 1200, height = 600, delay = 1)
  
  # Remove temporary image files
  file.remove(images)
}

animate()