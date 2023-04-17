pexc <- function(df, volume_column) {
  df_sorted <- df[order(df[[volume_column]], decreasing = TRUE),]
  df_sorted$rank <- 1:nrow(df_sorted)
  df_sorted$pexc <- df_sorted$rank / (nrow(df_sorted) + 1)
  return(df_sorted)
}

predict_pexc <- function(df,volume_column, new_volume) {
  volume <- df[[volume_column]]
  exceedance_probability <- df$pexc
  # Use linear interpolation to predict exceedance probability for new_volume
  predicted_probability <- approx(x = volume, y = exceedance_probability, xout = new_volume, method = "linear",rule = 2)
  # Combine original data and new data with their corresponding exceedance probabilities
  predicted_probability <- data.frame(volume_original = new_volume, pexc = predicted_probability$y)
  
  return(predicted_probability)
}

seco_normal_humedo_years <- function(df, seco_threshold = 0.3,humedo_threshold = 0.7) {
  # Define the thresholds for seco, normal, and humedo categories
  
  df <- df %>%
    mutate(type_wy = case_when(
      pexc <= seco_threshold ~ "húmedo",
      pexc > seco_threshold & pexc < humedo_threshold ~ "normal",
      pexc >= humedo_threshold ~ "seco"
    ))
  return(df)
}
