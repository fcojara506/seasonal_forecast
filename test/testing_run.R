rm(list = ls())
source("base/Preprocess_data.R")
#input data from preprocess
data_input <- preprocess_data(month_initialisation = "oct",
                              wy_holdout = 2022,
                              mode = "both")

# source("base/Regression_model.R")
# # ensemble volume forecast
# data_fore = forecast_vol_ensemble(data_input = data_input,
#                                   method = "lm")
# #ensemble flow forecast
# source("base/Knn_model.R")
# q_fore = run_q_forecast(data_input = data_input,
#                         data_fore = data_fore)
# #export results
# source("utils/run_model_function.R")
# output = export_data(data_input = data_input,
#                      data_fore = data_fore,
#                      q_fore = q_fore,
#                      export = "scores")
