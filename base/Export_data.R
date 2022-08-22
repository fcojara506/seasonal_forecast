
export_all <- function(all_data) {
  library(glue)
  library("icesTAF")
  
  # output folder
  folder_output = glue("data_output/data_per_model/{all_data$args$catchment_code}/")
  # create folder if it does not exist
  mkdir(folder_output)
  
  # save into a file
  output = glue(
    "{folder_output}all_data_{all_data$args$catchment_code}_",
    "1st{all_data$args$month_initialisation}_{all_data$regression_model$modelInfo$label}_{all_data$plot_text$predictor_list_join}_",
    "{all_data$plot_text$volume_span_text}{all_data$wy_holdout}.RDS")
  
  output = gsub(" ", "", output, fixed = TRUE)
  
  saveRDS(object = all_data,file = output)
  
  return(all_data)
}

## export specific data
merge_variables <- function(...) {
  #merge all the lists 
  all_data =c(...)
  
  return(all_data)
}


# export volume data to platform
export_volume_platform <- function(data,data_fore) {
  
  wy_target =  data$info$wy_holdout

  # observations
  v_normal = mean(data$y_train$volume_mm)
  v_pasado = data$y_train[as.character(wy_target-1),]
  
  #forecast min, max, median
  v_promax = quantile(data_fore$y_ens_fore, probs = c(0.95))[[1]]
  v_promin = quantile(data_fore$y_ens_fore, probs = c(0.05))[[1]]
  v_pron   = quantile(data_fore$y_ens_fore, probs = c(0.5))[[1]]
  
  
  export_data_frame = data.frame(
    cuenca = data$info$catchment_code,
    cuenca_name = data$raw_data$attributes_catchment$gauge_name,
    vol_normal = v_normal %>% round(2),
    vol_pasado = v_pasado %>% round(2),
    vol_promax = v_promax %>% round(2),
    vol_promin = v_promin %>% round(2),
    vol_pron   = v_pron %>% round(2),
    year = wy_target,
    fecha_emision = data$plot_text$datetime_initialisation,
    comentarios = "version de prueba"
  )
  
  return(export_data_frame)
  
}

export_flow_platform <- function(data,q_fore) {
  
  # observations of the forecast horizon
  #q_normal = apply(data$q_train, 2,FUN= mean)
  #q_prior = data$q_train[as.character(data$info$wy_holdout-1),]
  #q_obs = data$q_test
  
  # observations previous months
  q_flows = data$raw_data$monthly_flows %>%
    subset( !(wy_simple %in% data$info$remove_wys)) %>% 
    select(-wym_str,-cod_cuenca) %>% 
    data.table() %>%
    dcast.data.table(wy_simple~wym,value.var = "Q_mm")
  
  q_flows_normal = q_flows %>% 
    subset( !(wy_simple %in% data$info$wy_holdout)) %>% 
    select(-wy_simple) %>% 
    apply(2,FUN= mean) %>% 
    t %>% data.frame()
  
  q_flows_prior = q_flows %>%
    subset(wy_simple == data$info$wy_holdout-1) %>% 
    select(-wy_simple)
  
  q_flows_obs = q_flows %>% 
    subset(wy_simple == data$info$wy_holdout) %>% 
    select(-wy_simple) 
  q_flows_obs = subset(q_flows_obs, select = which(as.numeric(names(q_flows_obs)) < data$time_horizon$month_initialisation_index))
  
  names(q_flows_normal) = data$time_horizon$months_wy
  names(q_flows_prior) = data$time_horizon$months_wy
  names(q_flows_obs) = data$time_horizon$months_before_initialisation
  
  q_flows = rbindlist(
    list(
      q_flows_normal,
      q_flows_prior,
      q_flows_obs
      ),
    fill=T
    )
  
  columns = c("cuenca","cuenca_name","fecha","q_pron","q_pron_min","q_pron_max","q_last_year","q_normal","q_obs","fecha_emision")
  # forecast
  flow_fore = 
    data.table(q_fore) %>%
    apply( 2 , quantile , probs = c(.05,0.50,0.95) , na.rm = TRUE,names=T ) %>% 
    rbind(q_flows,.,fill=T) %>% 
    round(2) %>% 
    as.matrix()%>% 
    t() %>% 
    data.frame(check.names = F) %>% 
    `rownames<-` (data$plot_text$wy_holdout_months) %>% #(data$plot_text$forecast_horizon_months)
    `colnames<-`(c("q_normal","q_last_year","q_obs","q_pron_min","q_pron","q_pron_max")) %>% 
    rownames_to_column(var = "fecha") %>%
    mutate(cuenca = data$info$catchment_code) %>%
    mutate(cuenca_name = data$raw_data$attributes_catchment$gauge_name) %>%
    mutate(fecha_emision = data$plot_text$datetime_initialisation) %>%
    select(columns) %>% 
    mutate(comentarios = "version de prueba")
    
  flow_fore[is.na(flow_fore)] <- -1
    
  return(flow_fore)
  }


