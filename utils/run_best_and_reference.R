rm(list = ls())

# Import required functions from the base folder
source("base/Preprocess_data.R")
source("base/Regression_model.R")
source("base/Knn_model.R")
source("base/Export_data.R")

# Define the forecast mode
forecast_mode <- "cv"
# Read catchment attributes from a CSV file
# all available catchments, no data 6008005, 7317005, 7355002, 8106001
catchments_attributes_filename = "data_input/attributes/attributes_49catchments_ChileCentral.csv" 

cod_cuencas = fread(catchments_attributes_filename) %>%
  #no hydrological model data
  subset(!(cod_cuenca %in% c(6008005, 7317005, 7355002, 8106001))) %>%
  #bad scores
  subset(!(cod_cuenca %in% c(7381001,4531002,4522002,4515002))) %>% 
  select(cod_cuenca) %>% unlist()


# Define months for initialization
months_initialisation <- c(5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3)

# Define a function to preprocess data, perform volume forecast and calculate scores
perform_forecast <- function(catchment_code,
                             month_initialisation,
                             forecast_mode,
                             predictor_list = NULL,
                             resampling_method = "LOOCV") {
  
  # Preprocess the data
  datetime_emission = lubridate::make_date(2022, month_initialisation)
  if (month_initialisation<4) {datetime_emission = datetime_emission %m+% months(12)}
  
  data_input <- preprocess_data(datetime_initialisation = datetime_emission ,
                                forecast_mode = forecast_mode,
                                catchment_code = catchment_code,
                                predictor_list = predictor_list,
                                remove_wys = c(2020,2021),
                                #water_units = waterunits(q="mm",y="mm"),
                                #y_transform = list(log_transform = T,plot_transform_predictant = F),
                                save_raw = T)
  
  # Perform volume forecast
  data_fore <- forecast_vol_ensemble(data_input = data_input,
                                     forecast_mode = forecast_mode,
                                     resampling_method = resampling_method)
  
  # Calculate and return scores
  export_data(data_input = data_input, data_fore = data_fore, export = 'scores')
}




# # Perform forecasts for BEST combinations
# scores <- lapply(cod_cuencas, function(catchment_code) {
#   lapply(months_initialisation, function(month_initialisation) {
#     # Read the best models for the given catchment
#     data_best_models <- readRDS(file = paste0("data_output/mejores_modelos_cuenca_mes/",catchment_code,"_may-mar.RDS"))
#     
#     # Perform forecasts for the best combination of predictors
#     best_combination = data_best_models$best_combination
#     # Get the best combination of predictors for the given catchment and month
#     best_combination <- best_combination[best_combination$month_initialisation == month_initialisation,]
#     
#     # Perform forecast for the best combination of predictors
#     perform_forecast(catchment_code,
#                      month_initialisation,
#                      forecast_mode,
#                      unlist(best_combination$predictors),
#                      resampling_method = "LOOCV")
#     
#   })
# }) %>% purrr::flatten()
# # Save the scores
# saveRDS(object = scores, file = "data_output/scores/RDS/scores_20230330.RDS")
# 
# 
# 
# 
# 
# 
# # Perform forecasts for REFERNCE
# scores_reference <- lapply(cod_cuencas, function(catchment_code) {
#   lapply(months_initialisation, function(month_initialisation) {
#     # Perform reference forecasts with only one predictor - 'STORAGE_last_1months'
#     perform_forecast(catchment_code,
#                      month_initialisation,
#                      forecast_mode,
#                      "STORAGE_mean_1months",
#                      resampling_method = "LOOCV")
#   })
# })%>% purrr::flatten()
# # Save the scores
# saveRDS(object = scores_reference, file = "data_output/scores/RDS/scores_reference_20230330.RDS")
# ################################
# 
# 
# 
# 





# Perform forecasts for BEST combinations
scores <- lapply(cod_cuencas, function(catchment_code) {
  lapply(months_initialisation, function(month_initialisation) {
    # Read the best models for the given catchment
    data_best_models <- readRDS(file = paste0("data_output/mejores_modelos_cuenca_mes/",catchment_code,"_may-mar.RDS"))
    
    # Perform forecasts for the best combination of predictors
    best_combination = data_best_models$best_combination
    # Get the best combination of predictors for the given catchment and month
    best_combination <- best_combination[best_combination$month_initialisation == month_initialisation,]
    
    # Perform forecast for the best combination of predictors
    perform_forecast(catchment_code,
                     month_initialisation,
                     forecast_mode,
                     unlist(best_combination$predictors),
                     resampling_method = "cv"
                     )
    
  })
}) %>% purrr::flatten()
# Save the scores
saveRDS(object = scores, file = "data_output/scores/RDS/scores_20230330_cv2k.RDS")






# Perform forecasts for REFERNCE
scores_reference <- lapply(cod_cuencas, function(catchment_code) {
  lapply(months_initialisation, function(month_initialisation) {
    # Perform reference forecasts with only one predictor - 'STORAGE_last_1months'
    perform_forecast(catchment_code,
                     month_initialisation,
                     forecast_mode,
                     "STORAGE_mean_1months",
                     resampling_method = "cv")
  })
})%>% purrr::flatten()
# Save the scores
saveRDS(object = scores_reference, file = "data_output/scores/RDS/scores_reference_20230330_cv2k.RDS")
