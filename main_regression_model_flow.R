rm(list = ls())
#directory = "/Users/fco/CAPTA/Pronostico_estacional/"
#setwd(directory)

source("Preprocess_data.R")
source("Regression_model.R")
source("Knn_model.R")
source("Charts.R")
source("Export_data.R")
#source("run_model_function.R")
### Forecasts


forecast_flow =
  function(month_target,
           catchment_code = '5410002',
           month_initialisation = "sep",
           region = "ChileCentral_ens30avg",
           wy_holdout = 1992) {
    
    data = preprocess_data(
      catchment_code = catchment_code,
      region = region,
      month_initialisation = month_initialisation,
      horizon_month_start = month_target,
      horizon_month_end = month_target,
      horizon_strategy = "fixed",
      predictor_list =
        c("pr_sum_-1months"),
      wy_holdout = wy_holdout,
      remove_wys = c(2020, 2021)
    )
    
    # ensemble volume forecast
    data_fore =
      forecast_vol_ensemble(
        data = data,
        method = "lm",
        #ridge, lm, rlm, simpls,
        tuneLength = 10,
        preProcess = c("center", "scale"),
        n_members = 1000
      )
    
    q_fore = data_fore$y_ens_fore
    colnames(q_fore) = data$time_horizon$months_forecast_period
    
    return(data.frame(q_fore))
  }

flow_to_vol <- function(month_initial,...) {
  
  months_forecast_period =
    forecast_horizon(month_initial,"dynamic")$
    months_forecast_period
  
  q_fore = list()
  for (month_forecast in months_forecast_period) {
    q_fore[[month_forecast]] = 
      forecast_flow(month_target = month_forecast,...)
  }
  q_fore = do.call(cbind,q_fore)
    
  
  y_ens_fore = apply(q_fore, 1, sum) %>%
    as.matrix()
  
  return(list(
    q_fore = q_fore,
    y_ens_fore = y_ens_fore
    ))
}

results_new = flow_to_vol(month_initial = "sep")



# ###############3
# results = run_model(
#   catchment_code = '5410002',
#   month_initialisation = month_initial,
#   region = "ChileCentral_ens30avg",
#   wy_holdout = 1992
# )
#
# vol = data.frame(
#   results_new$y_ens_fore,
#   results$data_fore$y_ens_fore
# )
# colnames(vol) = c("q_to_v","v_to_q")
# vol = vol %>% reshape2::melt()
#
# library(ggplot2)
# ggplot(data = vol)+
#   geom_boxplot(aes(x=variable,y=value))+
#   geom_hline(yintercept = results$data_input$y_test$volume_mm)
