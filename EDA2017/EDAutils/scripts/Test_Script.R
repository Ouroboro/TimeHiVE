library(EDAutils)
library(ggplot2)

### SINGLE SERIES ###
rm(list = ls()) #CLEAN

# 1. LOAD DATA
data <- read_ts_data("./tests/testthat/fixtures/series2.txt")
data <- read_ts_data("./scripts/JALLY/tes3.txt")
data <- rnorm(400)

# 2. ANALYSIS
results <- eda_single(data)

# 3. PLOT
output_file <- "output_plot.png"
p <- eda_plots(results, output_file = output_file)

# 4. OUTPUT
print(p)
message("Plot saved in: ", normalizePath(output_file))

### COUPLED SERIES ###
rm(list = ls()) #CLEAN

# 1. LOAD DATA
series1 <- scan("scripts/Serie2.txt")
series2 <- scan("scripts/Serie3.txt")

series1 <- scan("./scripts/JALLY/rayo.txt")
series2 <- scan("./scripts/JALLY/tmax.txt")

series1 <- rnorm(400)
series2 <- rnorm(400)

# 2. ANALYSIS
#results <- eda_double(series1, series2, alternative = "greater")
results <- eda_double(series1, series2) #alternative = "two.sided" DEFAULT

# 3. PLOT
output_file <- "output_plot_corr.png"
p <- eda_plotd(results, output_file = output_file)

# 4. OUTPUT
print(p)
message("Plot saved in: ", normalizePath(output_file))

###    PSYCHROCLIMOGRAPH    ###
rm(list = ls()) #CLEAN

### FROME CSV ARPA STANDARD ###

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

### FROM TXT FILES ###

# Read and process data files
humidity_data1 <- read.table("scripts/JALLY/humr.txt", header = FALSE)
temperature_data1 <- read.table("scripts/JALLY/tmoy.txt", header = FALSE)

combined_data <- data.frame(
  humidity_data1,
  temperature_data1)
colnames(combined_data) <- c("rel_humidity", "temperature")

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