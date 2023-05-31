# secuencia de pasos para
# correr el modelo estadistico para una o varias cuencas



pronostico_operativo_unacuenca <-
  function(catchment_code, fecha_emision_Y_M_D, exportar_figuras = F) {
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
    
    if (exportar_figuras) {
      a = plotear_todas_figuras_pronosticos(data_input,data_fore, q_fore)
    }
    
    return(list(
      resultados_plataforma = plataforma,
      resultados_tecnicos = resultados
    ))
  }

pronostico_operativo <- function(codigos_cuencas,fecha_emision_Y_M_D,exportar_figuras = F) {
    # inicializar lista para guardar datos
    r_tecnicos = list()
    r_plataforma = list()
    
    # hacer loop para cada cuenca
    for (catchment_code in as.character(codigos_cuencas)) {
        print(paste("calculando pronóstico","cuenca",catchment_code," emision:", fecha_emision_Y_M_D))
        # ocupar funcion para una cuenca
        resultados = pronostico_operativo_unacuenca(catchment_code, fecha_emision_Y_M_D,exportar_figuras)
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
    list(plataforma_volumen = r_plataforma_vol,
         plataforma_caudal = r_plataforma_q,
         resultados_tecnicos = r_tecnicos
         )
    
    return(final)
    
}



plotear_todas_figuras_pronosticos <- function(data_input,data_fore,q_fore) {
  message("Exportar figuras puede tomar tiempo")
  message("Algunos mensajes de 'warnings' pueden aparecer con los gráficos")
  
  source("base/Charts.R")
  
  catchment_code = data_input$info$catchment_code
  month_initialisation = data_input$info$datetime_initialisation
  #### charts
  p1 = plot_X_y_train(data_input)
  
  ggsave(
    glue(
      "data_output/figuras/por_cuenca/1_scatter_predictoresVSvolumen/scatter_X_y_{catchment_code}_{month_initialisation}.png"
    ),
    plot = p1,
    width = 5,
    height = 5,
    dpi = 400,
  )
  
  #scatter volume of simulated vs observed in cross-validation
  p2 =
    plot_vol_sim_obs(data_input = data_input,
                     data_fore = data_fore)
  
  ggsave(
    glue(
      "data_output/figuras/por_cuenca/2_scatter_simulacionVSobservacion/scatter_y-sim-obs_{catchment_code}_{month_initialisation}.png"
    ),
    plot = p2,
    width = 5,
    height = 5,
    dpi = 400
  )
  #probability of exceedance vs volume, type of year traffic light
  p3 = plot_pexc_forecast(data_input, data_fore)
  
  
  ggsave(
    glue(
      "data_output/figuras/por_cuenca/3_probabilidad_excedencia_tipo/pexc_y-sim_{catchment_code}_{month_initialisation}.png"
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
      "data_output/figuras/por_cuenca/4_pronostico_volumen_hindcast/hindcast_volumen_{catchment_code}_{month_initialisation}.png"
    ),
    plot = p4,
    width = 8,
    height = 5,
    dpi = 400
  )
  
  
  #hydrogram of forecasted mean monthly flows
  p5 = plot_knn_flow(data_input =  data_input,
                     q_ens_fore = q_fore)
  
  
  ggsave(
    glue(
      "data_output/figuras/por_cuenca/5_pronostico_caudales/pronostico_caudales_{catchment_code}_{month_initialisation}.png"
    ),
    plot = p5,
    width = 8,
    height = 5,
    dpi = 400
  )
  #flow forecast ensemble as spaghetti
  p6 = plot_flow_spaghetti(data_input =  data_input,
                           q_ens_fore = q_fore)
  
  ggsave(
    glue(
      "data_output/figuras/por_cuenca/5_pronostico_caudales/spaghetti/forecast_flow_spagheti_{catchment_code}_{month_initialisation}.png"
    ),
    plot = p6,
    width = 8,
    height = 5,
    dpi = 400
  )
  # return(list(p1 = p1,
  #             p2 = p2,
  #             p3 = p3,
  #             p4 = p4,
  #             p5 = p5,
  #             p6 = p6,
  # ))
  return(NA)
}

# funcion para encontrar la fecha de emision mas reciente posible
encontrar_fecha_emision <- function(current_date = Sys.Date(),dias_retraso_entradas = 6) {
  
  
  if(day(current_date) >= 25 || day(current_date) <= 6) {
    warning("Se recomienda esperar hasta el 6 de mes entrante para actualizar datos de entrada")
  }
  
  current_month <- month(current_date)
  current_year <- year(current_date)
  
  if(day(current_date) >= dias_retraso_entradas) {
    closest_first_day <- ymd(paste(current_year, current_month, "01", sep = "-"))
  } else {
    previous_month <- current_month - 1
    if (previous_month == 0) {
      previous_month <- 12
      current_year <- current_year - 1
    }
    closest_first_day <- ymd(paste(current_year, previous_month, "01", sep = "-"))
  }
  return(closest_first_day)
}

