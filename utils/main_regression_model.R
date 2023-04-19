rm(list = ls())
#rename
source("base/Preprocess_data.R")
source("base/Regression_model.R")
source("base/Knn_model.R")
source("base/Charts.R")
source("base/Export_data.R")
source("base/Pexc.R")


forecast_mode = "both"

for (catchment_code in c(7321002, 5410002)) {
  # Define months for initialization
  
  for (month_initialisation in c(5, 9, 7)) {
    
    data_best_models = readRDS(
      file = paste0(
        "data_output/mejores_modelos_cuenca_mes/",
        catchment_code,
        "_may-mar.RDS"
      )
    )
    best_combination = data_best_models$best_combination
    best_combination = best_combination[best_combination$month_initialisation == month_initialisation, ]
    
    
    data_input <- preprocess_data(
      datetime_initialisation = lubridate::make_date(2013, month_initialisation),
      forecast_mode = forecast_mode,
      catchment_code = catchment_code,
      predictor_list = unlist(best_combination$predictors),
      save_raw = T
    )
    
    # ensemble volume forecast
    data_fore = forecast_vol_ensemble(data_input = data_input,
                                      forecast_mode = forecast_mode)
    
    
    # ensemble flow forecast
    q_ens_forecast =
      run_q_forecast(
        data_input = data_input,
        data_fore = data_fore,
        forecast_mode = forecast_mode
      )
    
    #### charts
    #scores = export_data(data_input, data_fore, export = "scores")$scores_volume
    p1 = plot_X_y_train(data_input)

    ggsave(
      glue(
        "data_output/presentaciones/scatter_X_y_{data_input$info$catchment_code}_{month_initialisation}.png"
      ),
      plot = p1,
      width = 5,
      height = 5,
      dpi = 400
    )
    
    #scatter volume of simulated vs observed in cross-validation
    p2 =
      plot_vol_sim_obs(data_input = data_input,
                       data_fore = data_fore)
    
    ggsave(
      glue(
        "data_output/presentaciones/scatter_y-sim-obs_{data_input$info$catchment_code}_{month_initialisation}.png"
      ),
      plot = p2,
      width = 5,
      height = 5,
      dpi = 400
    )
    #probability of exceedance vs volume
    p3 = plot_pexc_forecast(data_input, data_fore)
    
    ggsave(
      glue(
        "data_output/presentaciones/pexc_y-sim_{data_input$info$catchment_code}_{month_initialisation}.png"
      ),
      plot = p3,
      width = 5,
      height = 5,
      dpi = 400
    )
    # #ensemble volume in hindcast (cross-validation)
    p4 = plot_backtest_volume(data_input =  data_input,
                              data_fore = data_fore)
    ggsave(
      glue(
        "data_output/presentaciones/hindcast-vol_{data_input$info$catchment_code}_{month_initialisation}.png"
      ),
      plot = p4,
      width = 8,
      height = 5,
      dpi = 400
    )
    
    
    #hydrogram of forecasted mean monthly flows
    p5 = plot_knn_flow(data_input =  data_input,
                       q_ens_fore = q_ens_forecast)
    
    ggsave(
      glue(
        "data_output/presentaciones/forecast_flow_{data_input$info$catchment_code}_{month_initialisation}.png"
      ),
      plot = p5,
      width = 8,
      height = 5,
      dpi = 400
    )
    #flow forecast ensemble as spaghetti
    p6 = plot_flow_spaghetti(data_input =  data_input,
                             q_ens_fore = q_ens_forecast)
    ggsave(
      glue(
        "data_output/presentaciones/forecast_flow_spagheti_{data_input$info$catchment_code}_{month_initialisation}.png"
      ),
      plot = p6,
      width = 8,
      height = 5,
      dpi = 400
    )
  }
}
