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
catchments_attributes_filename = "data_input/attributes/attributes_45catchments_ChileCentral.csv" 

cod_cuencas = fread(catchments_attributes_filename)$cod_cuenca


# Define months for initialization
months_initialisation <- c(5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3)

# Define a function to preprocess data, perform volume forecast and calculate scores
perform_forecast <- function(catchment_code,
                             month_initialisation,
                             forecast_mode,
                             predictor_list = NULL,
                             flow_units = waterunits(q = "m^3/s", y = "GL")
                             ) {
  print(paste(month_initialisation,catchment_code))
  # Preprocess the data
  datetime_emission = lubridate::make_date(2022, month_initialisation)
  
  if (month_initialisation<4) {datetime_emission = datetime_emission %m+% months(12)}
  
  data_input <- preprocess_data(datetime_initialisation = datetime_emission ,
                                forecast_mode = forecast_mode,
                                catchment_code = catchment_code,
                                predictor_list = predictor_list,
                                remove_wys = c(2020,2021),
                                water_units = flow_units,
                                save_raw = T)
  
  # Perform volume forecast
  data_fore <- forecast_vol_ensemble(data_input = data_input,
                                     forecast_mode = forecast_mode)
  
  #flow forecast
  q_fore = run_q_forecast(
    data_input = data_input,
    data_fore = data_fore)
  
  # Calculate and return scores
  export_data(data_input = data_input,
              data_fore = data_fore,
              q_fore = q_fore,
              export = 'scores'
              )

}



# Perform forecasts for BEST combinations
scores <- lapply(cod_cuencas, function(catchment_code) {
  lapply(months_initialisation, function(month_initialisation) {

    # Perform forecast for the best combination of predictors
    perform_forecast(catchment_code = catchment_code,
                     month_initialisation = month_initialisation,
                     forecast_mode = forecast_mode,
                     predictor_list = get_best_predictors(catchment_code,month_initialisation),
                     flow_units = waterunits(q = "m^3/s", y = "GL"))

  })
}) %>% purrr::flatten()
# Save the scores
saveRDS(object = scores, file = "data_output/scores/RDS/scores_best_20230431.RDS")


# Perform forecasts for REFERENCE
scores_reference <- lapply(cod_cuencas, function(catchment_code) {
  lapply(months_initialisation, function(month_initialisation) {

    perform_forecast(catchment_code = catchment_code,
                     month_initialisation = month_initialisation,
                     forecast_mode = forecast_mode,
                     predictor_list = "STORAGE_mean_1months",
                     flow_units = waterunits(q = "m^3/s", y = "GL"))
    
  })
})%>% purrr::flatten()
# Save the scores
saveRDS(object = scores_reference, file = "data_output/scores/RDS/scores_reference_20230431.RDS")
################



######### compute using mm units
######### compute using mm units
######### compute using mm units
######### compute using mm units

# Perform forecasts for BEST combinations
scores <- lapply(cod_cuencas, function(catchment_code) {
  lapply(months_initialisation, function(month_initialisation) {
    
    # Perform forecast for the best combination of predictors
    perform_forecast(catchment_code = catchment_code,
                     month_initialisation = month_initialisation,
                     forecast_mode = forecast_mode,
                     predictor_list = get_best_predictors(catchment_code,month_initialisation),
                     flow_units = waterunits(q = "mm/month", y = "mm"))
    
  })
}) %>% purrr::flatten()
# Save the scores
saveRDS(object = scores, file = "data_output/scores/RDS/scores_best_20230431_mm.RDS")


# Perform forecasts for REFERENCE
scores_reference <- lapply(cod_cuencas, function(catchment_code) {
  lapply(months_initialisation, function(month_initialisation) {
    
    perform_forecast(catchment_code = catchment_code,
                     month_initialisation = month_initialisation,
                     forecast_mode = forecast_mode,
                     predictor_list = "STORAGE_mean_1months",
                     flow_units = waterunits(q = "mm/month", y = "mm"))
    
  })
})%>% purrr::flatten()
# Save the scores
saveRDS(object = scores_reference, file = "data_output/scores/RDS/scores_reference_20230431_mm.RDS")
################

