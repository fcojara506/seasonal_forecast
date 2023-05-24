
pronostico_operativo <- function(catchment_code,datetime_initialisation) {
  
  # cargar funciones de entrada para modelo estadistico
  source(file = "base/Preprocess_data.R")
data_input <- preprocess_data(
  catchment_code = catchment_code,
  datetime_initialisation = datetime_initialisation,
  predictor_list = get_best_predictors(catchment_code,lubridate::month(datetime_initialisation)),
  forecast_mode = "both"
)

# pronostico del volumen
source("base/Regression_model.R")
data_fore = forecast_vol_ensemble(data_input = data_input)


#pronostico de caudales
source("base/Knn_model.R")
q_fore =
  run_q_forecast(
    data_input = data_input,
    data_fore = data_fore
  )

# exportar datos
source("base/Export_data.R")
resultados =
  export_data(data_input = data_input,
              data_fore = data_fore,
              q_fore = q_fore,
              export = 'all')

plataforma = 
  export_platform(
    data_input = data_input,
    data_fore = data_fore,
    q_fore = q_fore,
    comentarios = "version 0.1"
    )
}
  
