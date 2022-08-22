rm(list = ls())

source("base/Preprocess_data.R")
source("base/Regression_model.R")
source("base/Knn_model.R")
source("base/Charts.R")
source("base/Export_data.R")

### Forecasts

# input data
data = 
  preprocess_data(
  catchment_code = '5410002',
  region = c("ChileCentral",
             "ens30avg",
             "SAC_EVDSep"),
  month_initialisation = "ene",
  horizon_strategy = "dynamic",
  predictor_list = 
  c(
   "pr_sum_-1months",
   #"AE_sum_1months",
   #"PROD_sum_1months",
   #"ROUT_sum_1months",
   #"SLZ_last_1months",
   #"SM_last_1months",
   "SP_sum_3months"
   #"SUZ_last_1months"
    ),
  wy_holdout = 2012,
  remove_wys = c(2020)
)



# ensemble volume forecast
data_fore = 
  forecast_vol_ensemble(
  data = data,
  method = "lm", #ridge, lm, rlm, simpls,
  tuneLength = 10,
  preProcess = c("center", "scale"), 
  n_members = 1000
  )

#### metrics
scores_volume = 
  y_scores(
    data_fore = data_fore,
    data = data)

# ensemble flow forecast
q_fore =
  q_ensemble(
  data = data,
  data_fore = data_fore,
  n_neighbors = 6,
  weight_method = 'distance'
  )


#df_platform_vol = export_volume_platform(data=data,data_fore=data_fore)
#df_platform_q   = export_flow_platform(data=data,q_fore = q_fore)

#### charts
#predictors vs target variable
p1=plot_X_y_train(
  data = data,
  export = T,
  show_chart = T
)

#scatter volume of simulated vs observed in cross-validation
p2=
  plot_vol_sim_obs(
  data = data,
  data_fore = data_fore,
  export = F,
  show_chart = T
)

#ensemble volume in hindcast (cross-validation)
p3=plot_backtest_volume(
  data = data,
  data_fore = data_fore,
  subplot = F,
  export = F,
  show_chart = T
  )


#hydrogram of forecasted mean monthly flows
p4=plot_knn_flow(
  data = data,
  q_fore = q_fore,
  export = F,
  show_chart = T
)


#final_model=data_fore$regression_model[[1]]$finalModel
#performance::check_model(final_model)

