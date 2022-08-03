rm(list = ls())
directory = "/Users/fco/CAPTA/Pronostico_estacional/"
setwd(directory)

source("Preprocess_data.R")
source("Regression_model.R")
source("Knn_model.R")
source("Charts.R")
source("Export_data.R")

### Forecasts
# input data

months_horizon = 
  c('sep',
    'oct',
    'nov',
    'dic',
    'ene',
    'feb',
    'mar'
    )
month_target = "oct"

data = 
  preprocess_data(
    catchment_code = '5410002',
    region = "ChileCentral_ens",
    month_initialisation = "oct",
    horizon_month_start = month_target,
    horizon_month_end = month_target,
    horizon_strategy = "fixed",
    predictor_list = 
      c(
        "pr_sum_-1months"
        #"tem_sum_-1months"
      ),
    wy_holdout = 2019,
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

#### charts
#predictors vs target variable
p1=plot_X_y_train(
  data = data,
  export = F,
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

# export data
all_data = 
  merge_variables(
    info = list(data$info),
    #data_fore,
    #q_fore = list(q_fore),
    scores_volume = list(scores_volume)
  )

#library(performance)

#final_model=data_fore$regression_model[[1]]$finalModel
#performance::check_model(final_model)

