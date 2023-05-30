# ----------------------------------------------------------------------------
# Nombre del Proyecto: Pronóstico híbridos del volumen estacional en Chile Central 
# Autor(es): Francisco Jara
# Fecha de Finalización: 2023/04/31
# Contacto: pronostico.caudales@gmail.com
# GitHub: https://github.com/fcojara506
# ----------------------------------------------------------------------------

deterministic_scores <- function(y_true,y_pred,normalise = F) {
  library(caret)
  
  scores =
    list(
    rmse_avg = caret::RMSE(y_pred, y_true),
    r2_avg = caret::R2(y_pred, y_true),
    mae_avg = caret::MAE(y_pred, y_true),
    pbias_avg = mean((y_true - y_pred) / y_true)
  )
  
  if (normalise) {
    scores =
      list(
        rmse_avg = caret::RMSE(y_pred, y_true)/mean(y_true),
        r2_avg = caret::R2(y_pred, y_true),
        mae_avg = caret::MAE(y_pred, y_true)/mean(y_true),
        pbias_avg = mean((y_true - y_pred) / y_true)
      )
  }
  return(scores)
}

ensemble_scores <- function(y_train,y_ens) {
  
  library(caret)
  #ensemble scores
  
  obs_climate               <- rep(mean(y_train), times = length(y_train) ) %>% 
    as.matrix()

  
  CRPS_ens = verification::crpsDecomposition(obs = y_train,eps = y_ens)
  
  
  crps_climate              <- caret::MAE(pred = obs_climate,obs = y_train)
  crpss                     <- 1 - CRPS_ens$CRPS/crps_climate
  
  
  return(list(
    mae_obs = crps_climate,
    crps_ens = CRPS_ens$CRPS,
    crpss_climatology = crpss,
    crps_reliability = CRPS_ens$Reli,
    crps_potential = CRPS_ens$CRPSpot
    ))
  
}

y_scores <- function(data_fore,data_input) {
  ##### volume data
  y_train = data_input$y_train$volume_original
  ##### ensemble monthly average
  y_ens_cv_avg = apply(data_fore$y_ens_cv,MARGIN = 2,mean) %>% as.numeric()
  y_ens = t(data_fore$y_ens_cv) %>% as.matrix()
  
  ##### ensemble scores
  uni_scores = deterministic_scores(y_true = y_train,y_pred = y_ens_cv_avg, normalise = F)
  ens_scores = ensemble_scores(y_train = y_train, y_ens = y_ens)
  #### regression scores
  reg_scores = deterministic_scores(y_true = data_fore$regression_model$pred$obs,y_pred = data_fore$regression_model$pred$pred)
  
  ###### altogether
  scores = data.frame( c(uni_scores,ens_scores))
  return(scores)
}

q_scores <- function(q_fore,data_input) {
  
  #observed streamflow data
  q_train=data_input$q_train %>% rownames_to_column(var = "wy")
  
  #ensemble monthly scores
  q_ens  = lapply(names(q_fore$q_cv),
                     function(x) rownames_to_column(data.frame(wy = x,t(q_fore$q_cv[[x]])),var = "month")) %>% 
    rbindlist()
  
  
  ##### ensemble monthly average
  q_ens_avg = lapply(names(q_fore$q_cv),
                     function(x){
                       q = q_fore$q_cv[[x]]
                       avg_vector = t(apply(q,MARGIN = 2, mean))
                       df = data.frame(wy = x,avg_vector)
                       colnames(df)[-1] = colnames(q)
                       return(df)
                     }) %>% 
    rbindlist()
  
  # long variables (vectors)
  q_train_long = q_train %>% data.table() %>%  melt.data.table(id.vars = "wy",value.name = "q_train",variable.name = "month")
  q_ens_avg_long  = q_ens_avg %>%
    data.table() %>%
    melt.data.table(id.vars = "wy",
                    value.name = "q_avg_ens",
                    variable.name = "month")
  
  q_long = merge.data.table(q_train_long,q_ens_avg_long)
  # ensemble
  q_long_ens = merge.data.table(q_train_long,q_ens) %>%
    select(-wy,-month) %>% 
    as.matrix()
  
  # compute metrics
  uni_scores = deterministic_scores(y_true = q_long$q_train,y_pred = q_long$q_avg_ens)
  ens_scores = ensemble_scores(y_train = q_long_ens[,"q_train"], y_ens = q_long_ens[,!colnames(q_long_ens) %in% 'q_train'])
  
  scores = data.frame( c(uni_scores,ens_scores))
  return(scores)
}
