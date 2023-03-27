rm(list = ls())

# Import required functions from the base folder
source("base/Preprocess_data.R")
source("base/Regression_model.R")
source("base/Knn_model.R")
source("base/Charts.R")
source("base/Export_data.R")

# Define the forecast mode
forecast_mode <- "cv"
# Read catchment attributes from a CSV file
catchments_attributes_filename <- "data_input/attributes/attributes_49catchments_ChileCentral.csv" 
attributes_catchments <- read.csv(catchments_attributes_filename)[-c(32,40,45,49),]#13,14,15,
cod_cuencas <- attributes_catchments$cod_cuenca

# Define months for initialization
months_initialisation <- c(5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3)

# Define a function to preprocess data, perform volume forecast and calculate scores
perform_forecast <- function(catchment_code, month_initialisation, forecast_mode, predictor_list = NULL) {
  
  # Preprocess the data
  datetime_emission = lubridate::make_date(2022, month_initialisation)
  if (month_initialisation<4) {
    datetime_emission = datetime_emission %m+% months(12)
  }
  data_input <- preprocess_data(datetime_initialisation = datetime_emission ,
                                forecast_mode = forecast_mode,
                                catchment_code = catchment_code,
                                predictor_list = predictor_list,
                                remove_wys = c(2020,2021),
                                #y_transform = list(log_transform = T,plot_transform_predictant = F),
                                save_raw = T)
  
  # Perform volume forecast
  data_fore <- forecast_vol_ensemble(data_input = data_input, forecast_mode = forecast_mode)
  
  # Calculate and return scores
  export_data(data_input = data_input, data_fore = data_fore, export = 'scores')
}

# Perform forecasts for best combinations
scores <- lapply(cod_cuencas, function(catchment_code) {
  lapply(months_initialisation, function(month_initialisation) {
    # Read the best models for the given catchment
    data_best_models <- readRDS(file = paste0("data_output/mejores_modelos_cuenca_mes/",catchment_code,"_may-mar.RDS"))
    
    # Perform forecasts for the best combination of predictors
    best_combination = data_best_models$best_combination
    # Get the best combination of predictors for the given catchment and month
    best_combination <- best_combination[best_combination$month_initialisation == month_initialisation,]
    
    # Perform forecast for the best combination of predictors
    perform_forecast(catchment_code, month_initialisation, forecast_mode, unlist(best_combination$predictors))
    
  })
}) %>% purrr::flatten()

# Save the scores
saveRDS(object = scores, file = "data_output/scores/RDS/scores_20230327.RDS")

# Perform forecasts for reference
scores_reference <- lapply(cod_cuencas, function(catchment_code) {
  lapply(months_initialisation, function(month_initialisation) {
    # Perform reference forecasts with only one predictor - 'STORAGE_last_1months'
    perform_forecast(catchment_code, month_initialisation, forecast_mode, "STORAGE_mean_1months")
  })
})%>% purrr::flatten()

# Save the scores
saveRDS(object = scores_reference, file = "data_output/scores/RDS/scores_reference_20230327.RDS")
