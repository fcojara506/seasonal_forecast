# ----------------------------------------------------------------------------
# Nombre del Proyecto: Pronóstico híbridos del volumen estacional en Chile Central 
# Autor(es): Francisco Jara
# Fecha de Finalización: 2023/04/31
# Contacto: pronostico.caudales@gmail.com
# GitHub: https://github.com/fcojara506
# ----------------------------------------------------------------------------

pexc <- function(df, volume_column = "volume_original") {
  
  df_sorted <- df[order(df[[volume_column]], decreasing = TRUE),]
  df_sorted$rank <- 1:nrow(df_sorted)
  df_sorted$pexc <- df_sorted$rank / (nrow(df_sorted) + 1)
  df_sorted = rownames_to_column(df_sorted,var = "wy_simple")
  
  return(df_sorted)
}

predict_pexc <- function(df,volume_column, new_volume) {
  df = as.data.frame(df)
  volume <- df[[volume_column]]
  exceedance_probability <- df$pexc
  new_volume = new_volume %>% data.table()
  new_volume = melt.data.table(new_volume, measure.vars = names(new_volume),
                      variable.name = "wy_simple",
                      value.name = "volume")
  # Use linear interpolation to predict exceedance probability for new_volume
  predicted_probability <- approx(x = volume,
                                  y = exceedance_probability,
                                  xout = new_volume$volume,
                                  method = "linear",
                                  rule = 2)
  # Combine original data and new data with their corresponding exceedance probabilities
  predicted_probability <- data.frame(predicted_probability) %>%
    cbind(new_volume$wy_simple) %>% 
    data.table()
  colnames(predicted_probability) = c("volume_original", "pexc","wy_simple")
  
  return(predicted_probability)
}

seco_normal_humedo_years <- function(df,
                                     seco_threshold = 0.3,
                                     humedo_threshold = 0.7) {
  # Define the thresholds for seco, normal, and humedo categories
  
  df <- df %>%
    mutate(type_wy = case_when(
      pexc <= seco_threshold ~ "húmedo",
      pexc > seco_threshold & pexc < humedo_threshold ~ "normal",
      pexc >= humedo_threshold ~ "seco"
    ))
  return(df)
}

score_type_year <- function(data_input, data_fore, univariable = FALSE) {
  # Load required libraries
  library(caret)
  
  # Prepare pexc_true and pexc_sim data
  pexc_true <- data_input$y_train_pexc %>%
    dplyr::select(wy_simple, type_wy_obs = type_wy)
  
  pexc_sim <- data_fore$pexc_ens_cv %>%
    dplyr::select(wy_simple, type_wy)
  
  # Apply univariable method if requested
  if (univariable) {
    pexc_sim <- pexc_sim %>%
      dplyr::count(wy_simple, type_wy) %>%
      dplyr::group_by(wy_simple) %>%
      dplyr::top_n(n = 1, wt = n) %>%
      dplyr::select(wy_simple, type_wy)
  }
  
  # Merge pexc_true and pexc_sim data
  df <- merge(pexc_true, pexc_sim, by = "wy_simple")
  
  # Convert 'type_wy' and 'type_wy_obs' to factors with the same levels
  common_levels <- unique(c(levels(factor(df$type_wy)), levels(factor(df$type_wy_obs))))
  df$type_wy <- factor(df$type_wy, levels = common_levels)
  df$type_wy_obs <- factor(df$type_wy_obs, levels = common_levels)
  
  # Create a confusion matrix
  cm <- confusionMatrix(df$type_wy, df$type_wy_obs)
  
  # Calculate performance metrics
  accuracy <- cm$overall[["Accuracy"]]
  precision <- data.frame(cm$byClass)["Precision"]
  recall <- data.frame(cm$byClass)["Recall"]
  f1_score <- data.frame(cm$byClass)["F1"]
  
  # Prepare results data frame
  results <- list(
    Accuracy = accuracy,
    Precision = precision$Precision,
    Recall = recall$Recall,
    F1 = f1_score$F1
  ) %>% data.frame()
  
  rownames(results) <- rownames(precision)
  return(results)
}


