rm(list = ls())
#rename
source("base/Preprocess_data.R")
source("base/Regression_model.R")
source("base/Knn_model.R")
source("base/Charts.R")
source("base/Export_data.R")


forecast_mode = "cv"
catchment_code = 5410002
### Forecasts
for (catchment_code in c(7321002, 5410002, 5710001,4503001,3820001)) {
  
for (month_initialisation in 5:9) {
  


  
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

# # ensemble flow forecast
# q_ens_fore =
#   run_q_forecast(
#   data_input = data_input,
#   data_fore = data_fore,
#   forecast_mode = forecast_mode
#   )

#### charts
#predictors vs target variable
p1=
  plot_X_y_train(
  data_input = data_input
)
ggsave(glue("data_output/figuras/scatter_xy/scatter_xy_{data_input$info$catchment_code}_{month_initialisation}.png"),plot=p1,width=6,height=4,dpi=400)


#scatter volume of simulated vs observed in cross-validation
p2=
  plot_vol_sim_obs(
  data_input = data_input,
  data_fore = data_fore
)
ggsave(glue("data_output/figuras/scatter_simobs/scatter_ysimobs_{data_input$info$catchment_code}_{month_initialisation}.png"),plot=p2,width=8,height=6,dpi=400)

# #ensemble volume in hindcast (cross-validation)

p3 = plot_backtest_volume(
  data_input = data_input,
  data_fore = data_fore,
  subplot = T
  )

ggsave(glue("data_output/figuras/hindcast_volumen/vol_hindcast_{data_input$info$catchment_code}_{month_initialisation}.png"),plot=p3,width=8,height=4,dpi=400)

}
}



