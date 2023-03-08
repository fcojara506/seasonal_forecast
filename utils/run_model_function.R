#rm(list = ls())

source("base/Preprocess_data.R")
source("base/Regression_model.R")
source("base/Knn_model.R")
source("base/Export_data.R")
source("base/Scores.R")

### Forecasts
# input data


run_model <- function(month_initialisation,forecast_mode,export,...) {
  
    #pre-processed data
    data_input = preprocess_data(
      month_initialisation = month_initialisation,
      forecast_mode = forecast_mode,
      ...)
    # ensemble volume forecast
    data_fore = forecast_vol_ensemble(
      data_input = data_input,
      forecast_mode = forecast_mode)
    # # ensemble flow forecast
    q_fore = run_q_forecast(
      data_input = data_input,
      data_fore = data_fore,
      forecast_mode = forecast_mode)
    #export results
    output = export_data(
      data_input = data_input,
      data_fore = data_fore,
      q_fore = q_fore,
      export = export)
  
  return(output)
  
}

