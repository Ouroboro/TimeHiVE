# PsychroPlot.R
# A script to create psychrometric plots from temperature and humidity data

# Calculate specific humidity (g/kg) from relative humidity and temperature
# H: Relative humidity (%)
# T: Temperature (°C)
# Pressure: Atmospheric pressure (hPa, default: 1013.25)
calculate_specific_humidity <- function(H, T, Pressure = 1013.25) {
  # Calculate vapor pressure using Magnus formula
  vapor_pressure <- (11.7 ^ (8.07131 - 1730.63 / (T + 233.426))) * 1.1
  # Calculate mixing ratio
  mixing_ratio <- 622 * vapor_pressure / (Pressure - vapor_pressure)
  # Calculate specific humidity
  specific_humidity <- mixing_ratio * H / 100
  return(specific_humidity)
}

# Compute isoenthalpic lines (constant enthalpy lines)
# T: Temperature (°C)
# h: Enthalpy (kJ/kg)
compute_enthalpy_lines <- function(T, h) {
  c_air <- 1006  # Specific heat of dry air (J/kg·K)
  hlg <- 2501000  # Latent heat of vaporization (J/kg)
  cw <- 1860  # Specific heat of water vapor (J/kg·K)
  
  # Calculate humidity ratio
  w <- (h * 1000 - c_air * T) / (hlg + cw * T)
  
  # Calculate maximum possible humidity ratio (saturation)
  vapor_pressure <- (11.7 ^ (8.07131 - 1730.63 / (T + 233.426))) * 1.1
  w_max <- 622 * vapor_pressure / (1013.25 - vapor_pressure)
  
  # Set values above saturation to NA
  w[w * 1000 > w_max] <- NA
  T[w * 1000 > w_max] <- NA
  
  return(list(humidity_ratio = w, temperature = T))
}

# Compute mean seasonal parameters from temperature and humidity data
# T: Temperature vector (°C)
# H: Relative humidity vector (%)
calculate_seasonal_parameters <- function(T, H) {
  c_air <- 1.006  # Specific heat of dry air (kJ/kg·K)
  hlg <- 2501  # Latent heat of vaporization (kJ/kg)
  cw <- 1.860  # Specific heat of water vapor (kJ/kg·K)
  
  vapor_pressure <- (11.7 ^ (8.07131 - 1730.63 / (T + 233.426))) * 1.1
  mixing_ratio <- 622 * vapor_pressure / (1013.25 - vapor_pressure)
  specific_humidity <- mixing_ratio * H / 100
  enthalpy <- c_air * T + (specific_humidity / 1000) * (hlg + T * cw)
  
  return(c(
    mean_temp = mean(T, na.rm = TRUE),
    mean_rel_humidity = mean(H, na.rm = TRUE),
    mean_enthalpy = mean(enthalpy, na.rm = TRUE),
    mean_spec_humidity = mean(specific_humidity, na.rm = TRUE)
  ))
}

# Create data for constant relative humidity lines
# rel_humidity: Relative humidity value (%)
create_rh_line_data <- function(rel_humidity) {
  temperature_seq <- seq(-10, 50, by = 0.02)
  data.frame(
    temperature = temperature_seq,
    specific_humidity = calculate_specific_humidity(rel_humidity, temperature_seq)
  )
}

# Create data for constant enthalpy lines
# enthalpy: Enthalpy value (kJ/kg)
create_enthalpy_line_data <- function(enthalpy) {
  temperature_seq <- seq(-10, 50, by = 0.02)
  enthalpy_values <- compute_enthalpy_lines(temperature_seq, enthalpy)
  w <- enthalpy_values$humidity_ratio
  T <- enthalpy_values$temperature
  
  finite_mask <- !is.na(T) & !is.na(w)
  data.frame(
    temperature = T[finite_mask],
    specific_humidity = 1000 * w[finite_mask]
  )
}

# Generate all temperature-SH combinations for the grid
generate_temperature_sh_grid <- function() {
  temperature_seq <- seq(-9.5, 49.5, by = 0.5)
  result_list <- list()
  
  for(temp in temperature_seq) {
    if(temp %% 1 == 0.5) {
      sh_seq <- seq(0.5, 49.5, by = 1)  # Specific humidity starts from 0.5
    } else {
      sh_seq <- seq(1, 50, by = 1)      # Specific humidity goes up to 50
    }
    temp_df <- data.frame(temperature = temp, specific_humidity = sh_seq)
    result_list[[length(result_list) + 1]] <- temp_df
  }
  
  final_combinations <- do.call(rbind, result_list)
  rownames(final_combinations) <- NULL
  return(final_combinations)
}

# Process raw data and extract temporal components
# df: Input data frame with datetime information
process_time_data <- function(df, datetime_col = NULL, value_col = NULL) {
  # Se non specificato, assume la prima colonna come data/ora e la seconda come valori
  if(is.null(datetime_col)) datetime_col <- names(df)[2]
  if(is.null(value_col)) value_col <- names(df)[3]
  
  # Verifica che le colonne esistano
  if(!datetime_col %in% names(df)) stop("Colonna datetime non trovata")
  if(!value_col %in% names(df)) stop("Colonna valori non trovata")
  
  # Converti a formato datetime (prova più formati)
  df$datetime <- lubridate::parse_date_time(df[[datetime_col]], 
                                            orders = c("ymd HMS", "dmY HMS", "Ymd HMS", "ymd HM", "dmY HM", "Ymd HM"))
  
  # Estrai componenti temporali
  df$year <- lubridate::year(df$datetime)
  df$month <- lubridate::month(df$datetime)
  df$day <- lubridate::day(df$datetime)
  df$time <- format(df$datetime, "%H:%M")
  
  # Rinomina la colonna dei valori per consistenza
  names(df)[names(df) == value_col] <- "value"
  
  # Seleziona solo le colonne rilevanti
  result <- df[, c("year", "month", "day", "time", "value")]
  
  # Converti valori a numerico (se non lo sono già)
  result$value <- as.numeric(result$value)
  
  return(result)
}

# Count occurrences in specified temperature and humidity ranges
# grid_df: Data frame with temperature-SH grid
# data_df: Data frame with observed values
count_occurrences <- function(grid_df, data_df) {
  grid_df$occurrences <- 0
  
  for(i in 1:nrow(grid_df)) {
    current_temp <- grid_df$temperature[i]
    current_sh <- grid_df$specific_humidity[i]
    
    temp_range <- c(current_temp - 0.5, current_temp + 0.5)
    sh_range <- c(current_sh - 0.5, current_sh + 0.5)
    
    count <- sum(
      data_df$temperature >= temp_range[1] & 
        data_df$temperature <= temp_range[2] & 
        data_df$specific_humidity >= sh_range[1] & 
        data_df$specific_humidity <= sh_range[2],
      na.rm = TRUE
    )
    
    grid_df$occurrences[i] <- count
  }
  
  # Remove grid points with zero occurrences
  grid_df <- grid_df[grid_df$occurrences != 0, ]
  grid_df$specific_humidity <- grid_df$specific_humidity - 0.5
  
  return(grid_df)
}

# Main plot function
create_psychrometric_plot <- function(grid_df, temperature_data, rel_humidity_data) {
  # Calculate seasonal parameters
  params <- calculate_seasonal_parameters(temperature_data, rel_humidity_data)
  mean_temp <- params["mean_temp"]
  mean_rel_humidity <- params["mean_rel_humidity"]
  mean_enthalpy <- params["mean_enthalpy"]
  mean_spec_humidity <- params["mean_spec_humidity"]
  
  cmax <- ceiling(max(grid_df[3]) / 10) * 10
  # Create base plot
  psychro_plot <- ggplot(grid_df, aes(x = temperature, y = specific_humidity)) +
    geom_point(aes(color = occurrences, size = 1, stroke = 1), shape = 19) +
    scale_color_gradientn(
      colors = c("blue4", "blue", "cyan", "green", "yellow", "red", "red3"),
      limits = c(0, cmax),
      na.value = NA,
      name = "Occurrences"
    ) +
    theme_bw() +
    labs(
      title = "Psychrometric Chart: Temperature vs Specific Humidity",
      x = "Temperature (°C)", 
      y = "Specific Humidity (g/kg)"
    )
  
  # Add RH lines (10% to 100%)
  for (rh in seq(10, 100, by = 10)) {
    line_data <- create_rh_line_data(rh)
    psychro_plot <- psychro_plot + 
      geom_line(
        data = line_data, 
        aes(x = temperature, y = specific_humidity), 
        color = "grey82", 
        linewidth = 0.2
      )
  }
  
  # Add enthalpy lines (0 to 180 kJ/kg)
  for (enthalpy in seq(0, 180, by = 10)) {
    line_data <- create_enthalpy_line_data(enthalpy)
    
    psychro_plot <- psychro_plot + 
      geom_line(
        data = line_data, 
        aes(x = temperature, y = specific_humidity), 
        linetype = "dotted", 
        color = "grey82", 
        linewidth = 0.4
      )
    
    # Add enthalpy labels
    max_y_point <- line_data[which.max(line_data$specific_humidity), ]
    psychro_plot <- psychro_plot +
      geom_text(
        data = max_y_point,
        aes(x = temperature - 1.75, y = specific_humidity + 0.75, angle = -15),
        label = paste0(enthalpy, " kJ/kg"),
        vjust = -0.5,
        color = "gray24",
        size = 3.5
      )
  }
  
  # Add RH labels at specific positions
  psychro_plot <- psychro_plot +
    annotate("text", x = 48.5, y = 7.7, label = "10% RH", size = 5, color = "gray64", angle = 23) +
    annotate("text", x = 48.5, y = 42.4, label = "50% RH", size = 5, color = "gray64", angle = 62) +
    annotate("text", x = 38.5, y = 46.5, label = "100% RH", size = 5, color = "gray64", angle = 63)
  
  # Add average values annotation
  avg_values_text <- sprintf(
    paste(
      "+ Average Values:\n",
      "Temperature: %.2f °C\n",
      "Relative Humidity: %.2f%%\n",
      "Enthalpy: %.2f kJ/kg\n",
      "Specific Humidity: %.2f g/kg\n",
      "Total Measurements: %d"
    ),
    mean_temp, mean_rel_humidity, mean_enthalpy, mean_spec_humidity, length(temperature_data)
  )
  
  psychro_plot <- psychro_plot +
    annotate(
      "label",  # Using label instead of text for background
      x = -5, 
      y = 45, 
      label = avg_values_text, 
      color = "magenta", 
      size = 6,
      hjust = 0,
      vjust = 1,
      fill = "white",
      label.size = 0
    ) +
    annotate(
      "point",
      x = mean_temp,
      y = mean_spec_humidity,
      color = "magenta",
      size = 5,
      shape = 3,
      stroke = 2
    )
  
  # Customize theme elements
  psychro_plot <- psychro_plot +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 20),
      legend.position = "right",
      legend.key.height = unit(2, "cm"),
      legend.key.width = unit(1, "cm"),
      legend.title = element_text(size = 14, angle = 90, hjust = 0.5),
      legend.text = element_text(size = 14), 
      legend.title.position = "right",
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 14, margin = margin(t = 8)),
      axis.text.y = element_text(size = 14, margin = margin(r = 8)),
      panel.border = element_rect(fill = NA, color = "black", size = 1),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    coord_equal(ratio = 6/10, xlim = c(-10, 50), ylim = c(0, 50), expand = FALSE) +
    guides(size = "none")
  
  # Save plot
  ggsave(
    "psychrometric_plot.png",
    psychro_plot,
    width = 50,
    height = 30,
    units = "cm",
    dpi = 600,
    bg = "white"
  )
  
  return(psychro_plot)
}