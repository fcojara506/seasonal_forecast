rm(list = ls())
source("base/Preprocess_data.R")
#input data from preprocess
data_input <- test_preprocess()$data2
source("base/Regression_model.R")
# ensemble volume forecast
data_fore = forecast_vol_ensemble(data_input = data_input,mode = "both")
source("base/Knn_model.R")

q_fore = run_q_forecast(
    data_input = data_input,
    data_fore = data_fore,
    n_neighbours = 6,
    weight_method = 'distance',
    mode = "both"
  )
