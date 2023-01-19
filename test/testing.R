rm(list = ls())
source("base/Preprocess_data.R")
#input data from preprocess
data_input <- test_preprocess()$data3
source("base/Regression_model.R")
# ensemble volume forecast
data_fore = forecast_vol_ensemble(data_input = data_input,mode = "both")
#ensemble flow forecast
source("base/Knn_model.R")
q_fore = run_q_forecast(data_input = data_input,data_fore = data_fore,mode = "both")
#export results
source("utils/run_model_function.R")
output = export_data(data_input = data_input,
                     data_fore = data_fore,
                     q_fore = q_fore,
                     export = "scores")
