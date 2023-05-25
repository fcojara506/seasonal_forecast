# secuencia de pasos para
# correr el modelo estadistico para una o varias cuencas

pronostico_operativo_unacuenca <-
  function(catchment_code, fecha_emision_Y_M_D) {
    # cargar funciones de entrada para modelo estadistico
    source(file = "base/Preprocess_data.R")
    data_input <- preprocess_data(
      catchment_code = catchment_code,
      datetime_initialisation = fecha_emision_Y_M_D,
      predictor_list = get_best_predictors(catchment_code, lubridate::month(fecha_emision_Y_M_D)),
      forecast_mode = "both"
    )
    
    # pronostico del volumen
    source("base/Regression_model.R")
    data_fore = forecast_vol_ensemble(data_input = data_input)
    
    #pronostico de caudales
    source("base/Knn_model.R")
    q_fore =
      run_q_forecast(data_input = data_input,
                     data_fore = data_fore)
    
    # exportar datos
    source("base/Export_data.R")
    resultados =
      export_data(
        data_input = data_input,
        data_fore = data_fore,
        q_fore = q_fore,
        export = 'all'
      )
    # exportar pronostico en formato plataforma web
    plataforma =
      export_platform(
        data_input = data_input,
        data_fore = data_fore,
        q_fore = q_fore,
        comentarios = "version 0.1"
      )
    
    return(list(
      resultados_plataforma = plataforma,
      resultados_tecnicos = resultados
    ))
  }

pronostico_operativo <- function(codigos_cuencas,fecha_emision_Y_M_D) {
    # inicializar lista para guardar datos
    r_tecnicos = list()
    r_plataforma = list()
    
    # hacer loop para cada cuenca
    for (catchment_code in as.character(codigos_cuencas)) {
        print(paste("calculando ","cuenca",catchment_code," emision:", fecha_emision_Y_M_D))
        # ocupar funcion para una cuenca
        resultados = pronostico_operativo_unacuenca(catchment_code, fecha_emision_Y_M_D)
        #guardar resultados en listas
        r_tecnicos[[catchment_code]] = resultados$resultados_tecnicos 
        r_plataforma[[catchment_code]] = resultados$resultados_plataforma

    }
    ## juntar dataframes de pronosticos de volumen y caudal
    # en una sola tabla
    r_plataforma_ = r_plataforma %>% purrr::transpose()
    r_plataforma_vol = r_plataforma_$df_platform_vol %>% rbindlist()
    r_plataforma_q = r_plataforma_$df_platform_q %>% rbindlist()
    # guardar resultados en una lista
    final = 
    list(plataforma_vol = r_plataforma_vol,
         plataforma_q = r_plataforma_q,
         resultados_tecnicos = r_tecnicos
         )
    
    return(final)
    
  }
