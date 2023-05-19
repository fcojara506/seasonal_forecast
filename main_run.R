
# preproceso (descarga,limpieza,correccion sesgo) meteorológico
source(file = "base/MeteoPresente1_Request-CDO.R")
source(file = "base/MeteoPresente2_EscalaCuenca.R")
source(file = "base/MeteoPresente3_DiasSimilares.R")
source(file = "base/MeteoPresente4_BiasAdjEnsemble.R")

# correr modelo hidrológico
source(file = "base/SimulacionTUW1.R")

# preprocesar simulaciones del modelo hidrológico (diario a mensual)
source(file = "utils/convert_daily_to_monthly_storage.R")

# preproceso (descarga y limpieza) indices climáticos
source(file = "base/Climate_index.R")

# cargar funciones de entrada para modelo estadistico
source(file = "base/Preprocess_data.R")

#cargar atributos de las 45 cuencas

catchment_code = 7321002
datetime_initialisation = "2022-05-01"
predictor_list = "STORAGE_last_1months"

data_input <- preprocess_data(
  catchment_code = catchment_code,
  datetime_initialisation = datetime_initialisation,
  predictor_list = predictor_list,
  forecast_mode = "prediction"
)

# pronostico del volumen
source("base/Regression_model.R")
data_fore = forecast_vol_ensemble(data_input = data_input)


#pronostico de caudales
source("base/Knn_model.R")
q_ens_forecast =
  run_q_forecast(
    data_input = data_input,
    data_fore = data_fore
  )

# exportar datos
source("base/Export_data.R")
export_data(data_input = data_input,
            data_fore = data_fore,
            export = 'all')
