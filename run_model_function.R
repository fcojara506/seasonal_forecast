rm(list = ls())
#directory = "/Users/fco/CAPTA/Pronostico_estacional/"
#setwd(directory)

source("Preprocess_data.R")
source("Regression_model.R")
source("Knn_model.R")
source("Charts.R")
source("Export_data.R")

### Forecasts
# input data

run_model <- function(
    catchment_code = '5410002',
    month_initialisation = "nov",
    region = "ChileCentral_ens30avg",
    wy_holdout = 2019
    ) {
  
data = 
  preprocess_data(
    catchment_code = catchment_code ,
    region = region,
    month_initialisation = month_initialisation,
    horizon_strategy = "dynamic",
    predictor_list = 
      c(
        "pr_sum_-1months"#,
        #"tem_sum_-1months"
      ),
    wy_holdout = wy_holdout,
    remove_wys = c(2020,2021)
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

# ensemble flow forecast
q_fore =
  q_ensemble(
    data = data,
    data_fore = data_fore,
    n_neighbors = 6,
    weight_method = 'distance'
  )


df_platform_vol = export_volume_platform(data=data,data_fore=data_fore)
df_platform_q   = export_flow_platform(data=data,q_fore = q_fore)

#### metrics
scores_volume = 
  y_scores(
    data_fore = data_fore,
    data = data
    )



return(list(
  df_platform_vol = df_platform_vol,
  df_platform_q   = df_platform_q,
  scores_volume = scores_volume,
  info = data$info,
  q_fore = q_fore,
  data_fore = data_fore,
  data_input = data
  ))
}


# from flow to volume
forecast_flow =
  function(month_target,
           catchment_code = '5410002',
           month_initialisation = "sep",
           region = "ChileCentral_ens30avg",
           wy_holdout = 1992
           ) {
    
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

flow_to_vol <- function(month_initialisation,...) {
  
  months_forecast_period =
    forecast_horizon(month_initialisation,"dynamic")$
    months_forecast_period
  
  # ensemble flow forecast for each month
  q_fore = list()
  for (month_forecast in months_forecast_period) {
    q_fore[[month_forecast]] = 
      forecast_flow(month_target = month_forecast,
                    month_initialisation = month_initialisation,
                    ...)
  }
  q_fore = do.call(cbind,q_fore)
  
  # ensemble volume from flow
  y_ens_fore = apply(q_fore, 1, sum) %>%as.matrix()
  
  return(list(
    q_fore = q_fore,
    y_ens_fore = y_ens_fore
  ))
}

a=flow_to_vol(month_initialisation = "sep")
b=run_model()
