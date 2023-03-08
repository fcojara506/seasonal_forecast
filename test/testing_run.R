rm(list = ls())
source("base/Preprocess_data.R")
#input data from preprocess
data_input <- preprocess_data(datetime_initialisation = '2020-10-01',
                              forecast_mode = "both",
                              catchment_code = "5410002",
                              predictor_list = c("pr_sum_-1months","SOI_last_1months"))

source("base/Regression_model.R")
# ensemble volume forecast
data_fore = forecast_vol_ensemble(data_input = data_input,
                                   method = "lm")
#ensemble flow forecast
source("base/Knn_model.R")
q_fore = run_q_forecast(data_input = data_input,
                         data_fore = data_fore)
#export results
source("base/Export_data.R")
output = export_data(data_input = data_input,
                     data_fore = data_fore,
                     q_fore = q_fore,
                     export = "scores")
