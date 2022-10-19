rm(list = ls())
#rename
source("base/Preprocess_data.R")
source("base/Regression_model.R")
source("base/Knn_model.R")
source("base/Charts.R")
source("base/Export_data.R")
#source("base/Convert_units.R")

### Forecasts
for (catchment_code in c(7321002, 5410002, 5710001,3820001)) {
  
# input data
data_input = 
  preprocess_data(
  catchment_code = catchment_code, #7321002, 5410002, 5710001,3820001
  month_initialisation = "oct",
  horizon_strategy = "dynamic",
  predictor_list = c("STORAGE_last_1months"),
  horizon_month_start = "sep",
  horizon_month_end = "mar",
  wy_holdout = 2022,
  units_q = "m3/s",#"m^3/s",#"mm/month",#"m^3/s",
  units_y = "GL"#"GL"
)

# ensemble volume forecast
data_fore = 
  forecast_vol_ensemble(
  data_input = data_input,
  method = "lm", #ridge, lm, rlm, simpls,gamLoess
  tuneLength = 100,
  preProcess = c("center", "scale"), #"range" 
  n_members = 1000
  )

#monthly_flow = data_input$raw_data$monthly_flows
#### metrics
scores_volume =
  y_scores(
    data_fore = data_fore,
    data_input = data_input
    )

# ensemble flow forecast
q_ens_fore =
  q_ensemble(
  data_input = data_input,
  data_fore = data_fore,
  n_neighbors = 6,
  weight_method = 'distance'
  )

#df_platform_vol = export_volume_platform(data=data,data_fore=data_fore)
#df_platform_q   = export_flow_platform(data=data,q_ens_fore = q_ens_fore)

#### charts
#predictors vs target variable
p1=
  plot_X_y_train(
  data_input = data_input,
  export = T,
  show_chart = T
)
ggsave(glue("{data_input$info$catchment_code}_scatter_xy.png"),plot=p1,width=6,height=4,dpi=400)


#scatter volume of simulated vs observed in cross-validation
p2=
  plot_vol_sim_obs(
  data_input = data_input,
  data_fore = data_fore,
  export = F,
  show_chart = T
)
ggsave(glue("{data_input$info$catchment_code}_scatter_SimObs.png"),plot=p2,width=8,height=6,dpi=400)

# #ensemble volume in hindcast (cross-validation)
p3=
  plot_backtest_volume(
  data_input = data_input,
  data_fore = data_fore,
  subplot = F,
  export = F,
  show_chart = T
  )

ggsave(glue("{data_input$info$catchment_code}_vol_fore.png"),plot=p3,width=15,height=8,dpi=400)

#hydrogram of forecasted mean monthly flows
p4 =
  plot_knn_flow(
  data_input = data_input,
  q_ens_fore = q_ens_fore,
  export = F,
  show_chart = T
)

ggsave(glue("{data_input$info$catchment_code}_q_fore.png"),plot=p4,width=10,height=8,dpi=400)

}




