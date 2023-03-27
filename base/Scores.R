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
  library(SpecsVerification)
  library(caret)
  #ensemble scores
  
  obs_climate               <- rep(mean(y_train), times = nrow(y_train) ) %>% 
    as.matrix()
  
  crps_ensembles            <- mean(
    SpecsVerification::EnsCrps(
    ens=y_ens,
    obs=y_train,
    R.new=NA
    )
  ) 
  
  crps_climate              <- caret::MAE(pred = obs_climate,obs = y_train)
  crpss                     <- 1 - crps_ensembles/crps_climate
  
  return(list(
    mae_obs = crps_climate,
    crps_ens = crps_ensembles,
    crpss_climatology = crpss))
  
}

y_scores <- function(data_fore) {
  #volume data
  y_train = data_fore$regression_model$pred$obs %>% as.matrix()
  #ensemble monthly average
  y_ens_cv_avg = apply(data_fore$y_ens_cv,MARGIN = 2,mean) %>% as.numeric()
  y_ens = t(data_fore$y_ens_cv) %>% as.matrix()

  uni_scores = deterministic_scores(y_true = y_train,y_pred = y_ens_cv_avg, normalise = T)
  ens_scores = ensemble_scores(y_train = y_train, y_ens = y_ens)
  
  scores = data.frame( c(uni_scores,ens_scores))
  return(scores)
}

q_scores <- function(q_fore,data_input) {
  #streamflow data
  q_train=data_input$q_train
  
  #ensemble monthly scores
  q_ens = q_fore
 
  uni_scores = deterministic_scores(y_true = q_train,y_pred = q_ens_cv_avg)
  ens_scores = ensemble_scores(y_train = q_train, y_ens = q_ens)
  
  scores = data.frame( c(uni_scores,ens_scores))
  return(scores)
}
