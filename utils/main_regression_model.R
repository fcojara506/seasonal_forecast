rm(list = ls())
#rename
source("base/Preprocess_data.R")
source("base/Regression_model.R")
source("base/Knn_model.R")
source("base/Charts.R")
source("base/Export_data.R")


forecast_mode = "cv"
catchment_code = 5410002
month_initialisation = 8
### Forecasts
#for (catchment_code in c(7321002, 5410002, 5710001,3820001)) {
  
data_best_models = readRDS(
  file = paste0("data_output/mejores_modelos_cuenca_mes/",catchment_code,"_may-sep.RDS"))

best_combination = data_best_models$best_combination
best_combination = best_combination[best_combination$month_initialisation == month_initialisation,]

                      
data_input <- preprocess_data(
  datetime_initialisation = lubridate::make_date(2022, month_initialisation),
  forecast_mode = forecast_mode,
  catchment_code = catchment_code,
  predictor_list = unlist(best_combination$predictors),
  save_raw = T
)

# ensemble volume forecast
data_fore = forecast_vol_ensemble(
  data_input = data_input,
  forecast_mode = forecast_mode)

#### metrics
scores = export_data(
  data_input = data_input,
  data_fore = data_fore,
  export = 'scores')

# ensemble flow forecast
q_ens_fore =
  run_q_forecast(
  data_input = data_input,
  data_fore = data_fore,
  forecast_mode = forecast_mode
  )

#df_platform_vol = export_volume_platform(data=data,data_fore=data_fore)
#df_platform_q   = export_flow_platform(data=data,q_ens_fore = q_ens_fore)

#### charts
#predictors vs target variable
# p1=
#   plot_X_y_train(
#   data_input = data_input,
#   export = T,
#   show_chart = T
# )
# 
# ggsave(glue("{data_input$info$catchment_code}_scatter_xy.png"),plot=p1,width=6,height=4,dpi=400)


#scatter volume of simulated vs observed in cross-validation
# p2=
#   plot_vol_sim_obs(
#   data_input = data_input,
#   data_fore = data_fore,
#   export = F,
#   show_chart = T
# )
# ggsave(glue("{data_input$info$catchment_code}_scatter_SimObs.png"),plot=p2,width=8,height=6,dpi=400)

# #ensemble volume in hindcast (cross-validation)
p3=
  plot_backtest_volume(
  data_input = data_input,
  data_fore = data_fore,
  subplot = F,
  export = F,
  show_chart = T
  )

ggsave(glue("vol_hindcast_{data_input$info$catchment_code}.png"),plot=p3,width=15,height=8,dpi=400)

# #hydrogram of forecasted mean monthly flows
# p4 =
#   plot_knn_flow(
#   data_input = data_input,
#   q_ens_fore = q_ens_fore,
#   export = F,
#   show_chart = T
# )
# 
# ggsave(glue("{data_input$info$catchment_code}_q_fore.png"),plot=p4,width=10,height=8,dpi=400)






