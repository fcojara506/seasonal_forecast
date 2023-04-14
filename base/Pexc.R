exceedance_probability <- function(df, volume_column) {
  df_sorted <- df[order(df[[volume_column]], decreasing = TRUE),]
  df_sorted$rank <- 1:nrow(df_sorted)
  df_sorted$pexc <- df_sorted$rank / (nrow(df_sorted) + 1)
  return(df_sorted)
}

predict_exceedance_probability <- function(df,volume_column, new_volume) {
  volume <- df[[volume_column]]
  exceedance_probability <- df$pexc
  # Use linear interpolation to predict exceedance probability for new_volume
  predicted_probability <- approx(x = volume, y = exceedance_probability, xout = new_volume, method = "linear")
  return(predicted_probability$y)
}


#### usage
df = exceedance_probability(data_input$y_train,volume_column = "volume_original")
new_volume <- data_fore_boot$y_cv
predicted_probability <- predict_exceedance_probability(df,volume_column = "volume_original", new_volume)
# Combine original data and new data with their corresponding exceedance probabilities
new_data <- data.frame(volume_original = new_volume, pexc = predicted_probability)

# Calculate the median of the volume_original in new_data
new_data_median <- median(new_data$volume_original)

# Find the index of the row with the closest volume_original value to the median
closest_index <- which.min(abs(new_data$volume_original - new_data_median))

# Filter new_data to only include the row with the closest volume_original value to the median
new_data_filtered <- new_data[closest_index, ]

# Create error bars data
new_data_error <- data.frame(
  x = new_data_filtered$pexc,
  ymin = min(new_data$volume_original),
  ymax = max(new_data$volume_original)
)

# Create horizontal error bars data
new_data_horiz_error <- data.frame(
  y = new_data_filtered$volume_original,
  xmin = min(new_data$pexc),
  xmax = max(new_data$pexc)
)

# Create the ggplot
p <- ggplot() +
  geom_point(data = df, aes(x = pexc, y = volume_original), color = "blue") +
  geom_point(data = new_data_filtered, aes(x = pexc, y = volume_original), color = "red") +
  geom_errorbar(data = new_data_error, aes(x = x, ymin = ymin, ymax = ymax), width = 0.01) +
  geom_errorbarh(data = new_data_horiz_error, aes(y = y, xmin = xmin, xmax = xmax), height = 50)

# Print the ggplot
print(p)
