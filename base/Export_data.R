
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

replace_negative <- function(value,replace_for = 0) {
  if (value<0) {
    value = replace_for
  }
  return(value)
}
# export volume data to platform
export_volume_platform <- function(data,data_fore) {
  
  wy_target =  data$info$wy_holdout

  # observations
  v_normal = mean(data$y_train$volume)
  v_pasado = data$y_train[as.character(wy_target-1),]
  
  #forecast min, max, median
  v_promax = quantile(data_fore$y_ens_fore, probs = c(0.95))[[1]] 
  v_promin = quantile(data_fore$y_ens_fore, probs = c(0.05))[[1]] #%>% replace_negative()
  v_pron   = quantile(data_fore$y_ens_fore, probs = c(0.5))[[1]]#%>% replace_negative()
  
  
  export_data_frame = data.frame(
    cuenca = data$info$catchment_code,
    cuenca_name = data$raw_data$attributes_catchment$gauge_name,
    vol_normal = v_normal %>% round(2),
    vol_pasado = v_pasado %>% round(2),
    vol_promax = v_promax %>% round(2)%>% replace_negative(),
    vol_promin = v_promin %>% round(2)%>% replace_negative(),
    vol_pron   = v_pron %>% round(2)%>% replace_negative(),
    year = wy_target,
    fecha_emision = data$extra_info$datetime_initialisation,
    comentarios = "version de prueba"
  )
  
  return(export_data_frame)
  
}

na_df <- function(df_original) {
  if (all(is.na(df_original))) {
    
    df = matrix(data = NA,
                nrow = 1,
                ncol = ncol(df_original)) %>%
      data.frame()
    
    names(df) = names(df_original)
    df_original = df
  }
  
  return(df_original)
}

export_flow_platform <- function(data,q_fore) {
  
  
  # observations previous months
  q_flows = data$raw_data$monthly_flows %>%
    subset( !(wy_simple %in% data$info$remove_wys)) %>% 
    data.table() %>%
    dcast.data.table(wy_simple~wym,value.var = "Q_converted")
  
  q_flows_normal = q_flows %>% 
    subset( !(wy_simple %in% data$info$wy_holdout)) %>%
    na.omit() %>% 
    select(-wy_simple) %>% 
    apply(2,FUN= mean) %>%
    t %>% data.frame()
  
  q_flows_prior = q_flows %>%
    subset(wy_simple == data$info$wy_holdout-1) %>% 
    select(-wy_simple)
  
  q_flows_obs = q_flows %>% 
    subset(wy_simple == data$info$wy_holdout) %>% 
    select(-wy_simple) 
  
  #filter until date of emission 
  #q_flows_obs = subset(q_flows_obs,
  #                     select = which(as.numeric(names(q_flows_obs)) < data$time_horizon$month_initialisation_index))
  #names(q_flows_obs) = data$time_horizon$months_before_initialisation
  
  names(q_flows_obs) = data$time_horizon$months_wy
  names(q_flows_normal) = data$time_horizon$months_wy
  names(q_flows_prior) = data$time_horizon$months_wy
  
  
  q_flows_list = rbindlist(
    list(
      q_flows_normal %>% na_df,
      q_flows_prior %>% na_df,
      q_flows_obs%>% na_df
      ),
    fill=T
    )
  
  columns = c("cuenca","cuenca_name","fecha","q_pron","q_pron_min","q_pron_max","q_last_year","q_normal","q_obs","fecha_emision")
  # forecast
  flow_fore = 
    data.table(q_fore) %>%
    apply( 2 , quantile ,
           probs = c(.05,0.50,0.95) ,
           na.rm = TRUE,names=T ) %>% 
    rbind(q_flows_list,.,fill=T) %>% 
    round(2) %>% 
    as.matrix()%>% 
    t() %>%
    data.frame(check.names = F) %>% 
    `rownames<-` (data$extra_info$wy_holdout_months) %>% #(data$plot_text$forecast_horizon_months)
    `colnames<-`(c("q_normal","q_last_year","q_obs","q_pron_min","q_pron","q_pron_max")) %>% 
    rownames_to_column(var = "fecha") %>%
    mutate(cuenca = data$info$catchment_code) %>%
    mutate(cuenca_name = data$raw_data$attributes_catchment$gauge_name) %>%
    mutate(fecha_emision = data$extra_info$datetime_initialisation) %>%
    select(all_of(columns)) %>% 
    mutate(comentarios = "version de prueba")
    
  flow_fore[is.na(flow_fore)] <- -1
    
  return(flow_fore)
  }


