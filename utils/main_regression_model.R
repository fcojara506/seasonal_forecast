
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

if (export == "pronostico_caudal") {
  q_ens = lapply(names(q_fore$q_cv),
                 function(x) rownames_to_column(data.frame(catchment_code = catchment_code,wy = x,t(q_fore$q_cv[[x]])),var = "month")) %>%
    rbindlist()
  
  q_obs = data_input$q_train %>%
    rownames_to_column(var = "wy") %>%
    data.table() %>%
    melt.data.table(id.vars = "wy",value.name = "q_obs",variable.name = "month")
  
  export = merge(q_obs,q_ens)
}else{
  # export whatever you fancy: scores, all, forecasts, platform
  export = export_data(data_input = data_input,
                       data_fore = data_fore,
                       q_fore = q_fore,
                       export = export)
}


return(export)

 
}
