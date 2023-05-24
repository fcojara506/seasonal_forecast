
#rename
source("base/Preprocess_data.R")
source("base/Regression_model.R")
source("base/Knn_model.R")
#source("base/Charts.R")
source("base/Export_data.R")


# set parameters
#catchment_code = 5410002
#month_initialisation = 9
#forecast_mode = "both"

run_model <- function(
    catchment_code,
    month_initialisation,
    forecast_mode = "cv",
    export = "scores"
                      ) {
  

# load best combination of predictors
print(paste0("catchment code:",catchment_code,' month_initial:',month_initialisation))


  best_predictors = get_best_predictors(catchment_code,month_initialisation)

# set input data
datetime_emission = lubridate::make_date(2022, month_initialisation)
if (month_initialisation<4) {datetime_emission = datetime_emission %m+% months(12)}

data_input <- preprocess_data(
  datetime_initialisation = datetime_emission,
  catchment_code = catchment_code,
  remove_wys = c(2020,2021),
  predictor_list = best_predictors,
  save_raw = T,
  forecast_mode = forecast_mode,
)

# ensemble volume forecast
data_fore = forecast_vol_ensemble(
  data_input = data_input,
  forecast_mode = forecast_mode
  )

# ensemble flow forecast
q_fore = run_q_forecast(
  data_input = data_input,
  data_fore = data_fore,
  forecast_mode = forecast_mode
  )

# # export whatever you fancy: scores, all, forecasts, platform
 export = export_data(data_input = data_input,
                         data_fore = data_fore,
                         q_fore = q_fore,
                         export = export)
 
# q_ens = lapply(names(q_fore$q_cv),
#                           function(x) rownames_to_column(data.frame(catchment_code = catchment_code,wy = x,t(q_fore$q_cv[[x]])),var = "month")) %>% 
#    rbindlist()
# 
# q_obs = data_input$q_train %>% 
#   rownames_to_column(var = "wy") %>%
#   data.table() %>% 
#   melt.data.table(id.vars = "wy",value.name = "q_obs",variable.name = "month")
# 
# export = merge(q_obs,q_ens)

#export = data.table(catchment_code = catchment_code, data_fore$y_ens_cv)
return(export)

# #### charts
# #scores = export_data(data_input, data_fore, export = "scores")$scores_volume
# p1 = plot_X_y_train(data_input)
# print(p1)
# # ggsave(
# #   glue(
# #     "data_output/presentaciones/scatter_X_y_{data_input$info$catchment_code}_{month_initialisation}.png"
# #   ),
# #   plot = p1,
# #   width = 5,
# #   height = 5,
# #   dpi = 400
# # )
#
# #scatter volume of simulated vs observed in cross-validation
# p2 =
#   plot_vol_sim_obs(data_input = data_input,
#                    data_fore = data_fore)
# print(p2)
# # ggsave(
# #   glue(
# #     "data_output/presentaciones/scatter_y-sim-obs_{data_input$info$catchment_code}_{month_initialisation}.png"
# #   ),
# #   plot = p2,
# #   width = 5,
# #   height = 5,
# #   dpi = 400
# # )
# #probability of exceedance vs volume
# p3 = plot_pexc_forecast(data_input, data_fore)
#
# # ggsave(
# #   glue(
# #     "data_output/presentaciones/pexc_y-sim_{data_input$info$catchment_code}_{month_initialisation}.png"
# #   ),
# #   plot = p3,
# #   width = 5,
# #   height = 5,
# #   dpi = 400
# # )
# # #ensemble volume in hindcast (cross-validation)
# p4 = plot_backtest_volume(data_input =  data_input,
#                           data_fore = data_fore)
# print(p4)
# # ggsave(
# #   glue(
# #     "data_output/presentaciones/hindcast-vol_{data_input$info$catchment_code}_{month_initialisation}.png"
# #   ),
# #   plot = p4,
# #   width = 8,
# #   height = 5,
# #   dpi = 400
# # )
#
#
# #hydrogram of forecasted mean monthly flows
# p5 = plot_knn_flow(data_input =  data_input,
#                    q_ens_fore = q_ens_forecast)
#
# # ggsave(
# #   glue(
# #     "data_output/presentaciones/forecast_flow_{data_input$info$catchment_code}_{month_initialisation}.png"
# #   ),
# #   plot = p5,
# #   width = 8,
# #   height = 5,
# #   dpi = 400
# # )
# #flow forecast ensemble as spaghetti
# p6 = plot_flow_spaghetti(data_input =  data_input,
#                          q_ens_fore = q_ens_forecast)
# # ggsave(
# #   glue(
# #     "data_output/presentaciones/forecast_flow_spagheti_{data_input$info$catchment_code}_{month_initialisation}.png"
# #   ),
# #   plot = p6,
# #   width = 8,
# #   height = 5,
# #   dpi = 400
# # )
#
#
 
}
