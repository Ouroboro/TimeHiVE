library(EDAutils)
library(ggplot2)

# 1. Carica i dati
data <- read_ts_data("./tests/testthat/fixtures/series2.txt")

# 2. Esegui analisi
results <- eda_single(data)

# 3. Genera e salva plot
output_file <- "output_plot.png"
p <- eda_plots(results, output_file = output_file)

# 4. Visualizza
print(p)
message("Plot salvato in: ", normalizePath(output_file))

# Esempio di utilizzo
series1 <- rnorm(200)
series2 <- rnorm(200)

series1 <- scan("scripts/Serie2.txt")
series2 <- scan("scripts/Serie3.txt")

results <- eda_double(series1, series2, alternative = "greater")

output_file <- "output_plot_corr.png"
p <- eda_plotd(results, output_file = output_file)


# --------------------------
# Data Processing Section
# --------------------------

# Read and process data files
humidity_data <- read.csv("scripts/HR.csv")
temperature_data <- read.csv("scripts/TE.csv")

# Process both datasets
processed_humidity <- process_time_data(humidity_data)
processed_temperature <- process_time_data(temperature_data)

# Merge datasets
combined_data <- merge(
  processed_humidity,
  processed_temperature,
  by = c("year", "month", "day", "time"),
  all = TRUE,
  suffixes = c("_humidity", "_temperature")
)

# Sort chronologically
combined_data$datetime <- ymd_hm(paste(
  combined_data$year,
  combined_data$month,
  combined_data$day,
  combined_data$time,
  sep = "/"
))
combined_data <- combined_data[order(combined_data$datetime), ]
combined_data$datetime <- NULL

# Clean column names and data
colnames(combined_data) <- c(
  "year", "month", "day", "time",
  "rel_humidity", "temperature"
)

# Apply data filters
combined_data$rel_humidity[combined_data$rel_humidity < 0 | combined_data$rel_humidity > 100] <- NA
combined_data$temperature[combined_data$temperature < -10 | combined_data$temperature > 70] <- NA

# Calculate specific humidity
combined_data$specific_humidity <- calculate_specific_humidity(
  combined_data$rel_humidity,
  combined_data$temperature
)

# Generate temperature-SH grid and count occurrences
temperature_sh_grid <- generate_temperature_sh_grid()
temperature_sh_grid <- count_occurrences(temperature_sh_grid, combined_data)

# Create and save the psychrometric plot
psychro_plot <- create_psychrometric_plot(
  temperature_sh_grid,
  combined_data$temperature,
  combined_data$rel_humidity
)
