library(icesTAF)
library(glue)

source("base/Scores.R")
# replace_negative <- function(value,replace_for = 0) {
#   if (value<0) {
#     value = replace_for
#   }
#   return(value)
# }

# export volume data to platform
library('rlist')

export_volume_platform <- function(data_input,data_fore) {
  
  wy_target =  data_input$info$wy_holdout

  # observations
  v_normal = mean(data_input$y_train$volume)
  v_pasado = data_input$y_train[as.character(wy_target-1),]
  
  #forecast min, max, median
  v_promax = quantile(data_fore$y_ens_fore, probs = c(0.95))[[1]] 
  v_promin = quantile(data_fore$y_ens_fore, probs = c(0.05))[[1]] #%>% replace_negative()
  v_pron   = quantile(data_fore$y_ens_fore, probs = c(0.5))[[1]]#%>% replace_negative()
  
  
  export_data_frame = data.frame(
    cuenca = data_input$info$catchment_code,
    cuenca_name = data_input$raw_data$attributes_catchment$gauge_name,
    vol_normal = v_normal %>% round(2),
    vol_pasado = v_pasado %>% round(2),
    vol_promax = v_promax %>% round(2),#%>% replace_negative(),
    vol_promin = v_promin %>% round(2),#%>% replace_negative(),
    vol_pron   = v_pron %>% round(2),#%>% replace_negative(),
    year = wy_target,
    fecha_emision = data_input$time_horizon$datetime_initialisation,
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

export_flow_platform <- function(data_input,q_fore,remove_q_after_emission=T) {
  
  # observations previous months
  q_flows = data_input$raw_data$monthly_flows %>%
    subset( !(wy_simple %in% data_input$info$remove_wys)) %>% 
    data.table() %>%
    dcast.data.table(wy_simple~wym,value.var = "Q_converted")
  
  q_flows_normal = q_flows %>% 
    subset( !(wy_simple %in% data_input$info$wy_holdout)) %>%
    na.omit() %>% 
    select(-wy_simple) %>% 
    apply(2,FUN= mean) %>%
    t %>% data.frame()
  
  q_flows_prior = q_flows %>%
    subset(wy_simple == data_input$info$wy_holdout-1) %>% 
    select(-wy_simple)
  
  q_flows_obs = q_flows %>% 
    subset(wy_simple == data_input$info$wy_holdout) %>% 
    select(-wy_simple) 
  
  if (remove_q_after_emission) {
  #filter until date of emission 
  q_flows_obs = subset(q_flows_obs,
                      select = which(as.numeric(names(q_flows_obs)) < data_input$time_horizon$month_initialisation_index))
  names(q_flows_obs) = data_input$time_horizon$months_before_initialisation
  }
  
  names(q_flows_obs) = data_input$time_horizon$months_wy
  names(q_flows_normal) = data_input$time_horizon$months_wy
  names(q_flows_prior) = data_input$time_horizon$months_wy
  
  
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
    data.table(q_fore$q_predict[[names(q_fore$q_predict)]]) %>%
    apply( 2 , quantile ,
           probs = c(.05,0.50,0.95) ,
           na.rm = TRUE,
           names=T ) %>% 
    rbind(q_flows_list,.,fill=T) %>% 
    round(2) %>% 
    as.matrix()%>% 
    t() %>%
    data.frame(check.names = F) %>% 
    `rownames<-` (data_input$time_horizon$wy_holdout_months) %>% #(data_input$plot_text$forecast_horizon_months)
    `colnames<-`(c("q_normal","q_last_year","q_obs","q_pron_min","q_pron","q_pron_max")) %>% 
    rownames_to_column(var = "fecha") %>%
    mutate(cuenca = data_input$info$catchment_code) %>%
    mutate(cuenca_name = data_input$raw_data$attributes_catchment$gauge_name) %>%
    mutate(fecha_emision = data_input$time_horizon$datetime_initialisation) %>%
    select(all_of(columns)) %>% 
    mutate(comentarios = "version de prueba")
    
  flow_fore[is.na(flow_fore)] <- -1
    
  return(flow_fore)
  }

short_river_name <- function(var) {stringr::word(var,start = 1,end = 2)}

export_data <- function(data_input,
                        data_fore,
                        q_fore = NULL,
                        export = "all"
) {
  
  if (!is.character(export) || !export %in% c("scores", "platform","forecasts", "all")) {stop("Invalid mode. Should be either 'scores', 'platform', 'forecasts' or 'all'")}
  
  results = list(info = data_input$info)
  
  if (export == "scores" | export == "all") {
    # ensemble type of year 
    scores_year_classification_ens =  
      score_type_year(data_input,data_fore, univariable = FALSE)
   # univariable type of year
     scores_year_classification_uni =  
      score_type_year(data_input,data_fore, univariable = TRUE)
    # metrics
    scores_volume <- y_scores(data_fore = data_fore) %>% 
      cbind(data.frame(accuracy_ens = scores_year_classification_ens$Accuracy[[1]])) %>% 
      cbind(data.frame(accuracy_uni = scores_year_classification_uni$Accuracy[[1]]))
    
    results <- list.append(results,
                           scores_volume = scores_volume,
                           model_info = data_fore$model_info)
  }
  
  if (export == "platform" | export == "all") {
    #platform data
    df_platform_vol <- export_volume_platform(data_input=data_input,data_fore=data_fore)
    df_platform_q   <- export_flow_platform(data_input=data_input,q_fore = q_fore,remove_q_after_emission = F)
    results <- list.append(results,df_platform_vol = df_platform_vol,df_platform_q = df_platform_q)
  }
  
  if (export == 'forecasts' | export == "all" ) {
    #export the predictors, volume ensemble forecast and flow ensemble forecast
    results <- list.append(results, q_fore = q_fore,data_fore = data_fore,data_input = data_input)
  }
  
  return(results)
}

