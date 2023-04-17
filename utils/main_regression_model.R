rm(list = ls())
#rename
source("base/Preprocess_data.R")
source("base/Regression_model.R")
source("base/Knn_model.R")
source("base/Charts.R")
source("base/Export_data.R")
source("base/Pexc.R")


forecast_mode = "both"
catchment_code = 7321002

# Define months for initialization
month_initialisation <- 7 #c(5, 6, 7, 8, 9)


data_best_models = readRDS(file = paste0("data_output/mejores_modelos_cuenca_mes/",catchment_code,"_may-mar.RDS"))
best_combination = data_best_models$best_combination
best_combination = best_combination[best_combination$month_initialisation == month_initialisation,]

       
data_input <- preprocess_data(
  datetime_initialisation = lubridate::make_date(2016, month_initialisation),
  forecast_mode = forecast_mode,
  catchment_code = catchment_code,
  predictor_list = unlist(best_combination$predictors),
  save_raw = T
)

# ensemble volume forecast

data_fore = forecast_vol_ensemble(
  data_input = data_input,
  forecast_mode = forecast_mode)

#plot_pexc_forecast(data_input,data_fore)

q_ens_forecast =
  run_q_forecast(
  data_input = data_input,
  data_fore = data_fore,
  forecast_mode = forecast_mode
  )

# #ensemble volume in hindcast (cross-validation)
p3=plot_backtest_volume(
  data_input =  data_input,
  data_fore = data_fore,
  subplot = T
)

print(p3)
q_ens_fore = q_ens_forecast
#hydrogram of forecasted mean monthly flows
p4 = plot_knn_flow(
  data_input =  data_input,
  q_ens_fore = q_ens_forecast
)
print(p4)
# 
# # data_fore_bag = forecast_vol_ensemble(
# #   data_input = data_input,
# #   forecast_mode = forecast_mode,
# #   ensemble_method = "bagging"
# #   )
# 
# 
# model = data_fore_boot$regression_model$finalModel
# summary(model)
# #library(performance)
# #check_model(model)
# library(broom)
# model.diag.metrics <- augment(model)
# #par(mfrow = c(2, 2))
# #plot(model)
# par(mfrow = c(2, 3))
# 
# plot(model, 1)
# 
# plot(model,2)
# 
# #Homogeneity of variance
# plot(model, 3)
# 
# # Cook's distance
# plot(model, 4)
# # Residuals vs Leverage
# plot(model, 5)
# 
# 
# #### metrics
# 
# scores_best_boot = export_data(
#   data_input = data_input,
#   data_fore = data_fore_boot,
#   export = 'scores')$scores_volume
# 
# scores_best_bag = export_data(
#   data_input = data_input,
#   data_fore = data_fore_bag,
#   export = 'scores')$scores_volume
# 
# 
# #     
# #     data_input <- preprocess_data(
# #       datetime_initialisation = lubridate::make_date(2022, month_initialisation),
# #       forecast_mode = forecast_mode,
# #       catchment_code = catchment_code,
# #       predictor_list = "STORAGE_mean_1months",
# #       save_raw = T
# #     )
# #     
# #     # ensemble volume forecast
# #     data_fore = forecast_vol_ensemble(
# #       data_input = data_input,
# #       forecast_mode = forecast_mode)
# #     
# #     #### metrics
# #     scores_ref = export_data(
# #       data_input = data_input,
# #       data_fore = data_fore,
# #       export = 'scores')
# #     
# #   scores_ref_vol = scores_ref$scores_volume
# #     
# # 
# # #saveRDS(object = scores_reference ,"data_output/scores/RDS/scores_reference_20230324.RDS")
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # # # # ensemble flow forecast
# # # # q_ens_fore =
# # # #   run_q_forecast(
# # # #   data_input = data_input,
# # # #   data_fore = data_fore,
# # # #   forecast_mode = forecast_mode
# # # #   )
# # # 
# # # #### charts
# # # #predictors vs target variable
# # # p1=
# # #   plot_X_y_train(
# # #   data_input = data_input
# # # )
# # # ggsave(glue("data_output/figuras/scatter_xy/scatter_xy_{data_input$info$catchment_code}_{month_initialisation}.png"),plot=p1,width=6,height=4,dpi=400)
# # # 
# # # 
# # # #scatter volume of simulated vs observed in cross-validation
# # # p2=
# # #   plot_vol_sim_obs(
# # #   data_input = data_input,
# # #   data_fore = data_fore
# # # )
# # # ggsave(glue("data_output/figuras/scatter_simobs/scatter_ysimobs_{data_input$info$catchment_code}_{month_initialisation}.png"),plot=p2,width=8,height=6,dpi=400)
# # # 
# # # # #ensemble volume in hindcast (cross-validation)
# # # 
# # # p3 = plot_backtest_volume(
# # #   data_input = data_input,
# # #   data_fore = data_fore,
# # #   subplot = T
# # #   )
# # # 
# # # ggsave(glue("data_output/figuras/hindcast_volumen/vol_hindcast_{data_input$info$catchment_code}_{month_initialisation}.png"),plot=p3,width=8,height=4,dpi=400)
# 
