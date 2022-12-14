rm(list = ls())

source("base/Preprocess_data.R")
source("base/Regression_model.R")
source("base/Knn_model.R")
source("base/Charts.R")
source("base/Export_data.R")

### Forecasts
# input data
short_river_name <- function(var) {stringr::word(var,start = 1,end = 2)}

forecast_vol_to_flow <- function(...) {
  
data = preprocess_data(...)
# ensemble volume forecast
data_fore = forecast_vol_ensemble(data_input = data)

# ensemble flow forecast
q_fore =
  q_ensemble(
    data_input = data,
    data_fore = data_fore,
    n_neighbors = 6,
    weight_method = 'distance'
  )

return(
  list(
  q_fore = q_fore,
  data_fore = data_fore,
  data = data
  )
  )


}


# ensemble flow compute in regression
forecast_flow_monthly =
  function(month_target,
           ...
           ) {
    
    data = preprocess_data(
      horizon_month_start = month_target,
      horizon_month_end = month_target,
      horizon_strategy = "fixed",
      ...
    )
    # ensemble volume forecast
    data_fore = forecast_vol_ensemble(data = data)
    
    #streamflow for target year
    q_fore = data_fore$y_ens_fore
    colnames(q_fore) = data$time_horizon$months_forecast_period
    q_fore = data.frame(q_fore)
    
    #streamflow in retrospective
    q_ens_cv = data_fore$y_ens_cv
    
    return(
      list(
        q_fore = q_fore,
        q_ens_cv = q_ens_cv
      )
    )
  }
#compute ensemble volume from flows
forecast_flow_to_vol <- function(month_initialisation,...) {
  
  months_forecast_period =
    forecast_horizon(month_initialisation,"dynamic")$
    months_forecast_period
  
  # ensemble flow forecast for each month
  df = list()
  for (month_forecast in months_forecast_period) {
    df[[month_forecast]] = 
      
      forecast_flow_monthly(
        month_target = month_forecast,
        month_initialisation = month_initialisation,
        ...
        )
  }
  df = purrr::transpose(df)
  
  # streamflow for target year
  q_fore = do.call(cbind,df$q_fore)
  # ensemble volume from flow
  y_ens_fore = apply(q_fore, 1, sum)
  
  #ensemble volume in retrospective
  y_ens_cv= Reduce("+",df$q_ens_cv )
  
  # volume as normal
  data_input = preprocess_data(
    month_initialisation=month_initialisation,
    ...
    )
  
  #replace new volume forecast
  data_fore = list()
  data_fore$y_ens_fore = y_ens_fore
  data_fore$y_ens_cv = y_ens_cv
  
  return(
    list(
      q_fore = q_fore,
      data_fore = data_fore,
      data = data_input
    )
  )
}

export_data <- function(data,
                        data_fore,
                        q_fore
                          ) {
  #platform data
  df_platform_vol = export_volume_platform(data=data,data_fore=data_fore)
  df_platform_q   = export_flow_platform(data=data,q_fore = q_fore)
  
  #### metrics
  scores_volume = 
    y_scores(
      data_fore = data_fore,
      data = data
    )
  
  return(
    list(
      df_platform_vol = df_platform_vol,
      df_platform_q   = df_platform_q,
      scores_volume = scores_volume,
      info = data$info
      #q_fore = q_fore,
      #data_fore = data_fore,
      #data_input = data
    )
  )
}

run_model <- function(...,month_initialisation,direction = "vol_to_flow") {
  if (direction=="vol_to_flow") {
    
    output = 
      forecast_vol_to_flow(
        month_initialisation=month_initialisation,
        ...
        #month_initialisation = "oct",wy_holdout = 2022
        )
    
    }else if(direction=="flow_to_vol"){
      
  output = 
    forecast_flow_to_vol(
      month_initialisation=month_initialisation,
      ...
      )
  
    }
  output$data$info$direction = direction
  
  return(
    do.call(export_data,output)
    )
  
}

#tests
#a=run_model(month_initialisation = "oct",wy_holdout=1990,direction = "vol_to_flow")
#b=run_model(month_initialisation = "oct",wy_holdout=1990,direction = "flow_to_vol")

