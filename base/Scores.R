deterministic_scores <- function(y_true,y_pred) {
  library(hydroGOF)
  
  scores  = hydroGOF::gof.data.frame(
    sim = y_pred,
    obs = y_true)
  
  rmse            = scores["RMSE",][[1]]
  R2              = scores["R2",][[1]]
  
  return(list(
    rmse_det = rmse,
    r2_det = R2
  ))
}

ensemble_scores <- function(y_train,y_ens) {
  library(SpecsVerification)
  library(hydroGOF)
  #ensemble scores
  
  obs_climate               <- rep(mean(y_train), times = nrow(y_train)) %>% 
    as.matrix()
  
  crps_ensembles            <- mean(
    SpecsVerification::EnsCrps(
    ens=y_ens,
    obs=y_train,
    R.new=NA
    )
  ) 
  
  crps_climate              <- hydroGOF::mae(sim = obs_climate,obs = y_train)
  crpss                     <- 1 - crps_ensembles/crps_climate
  
  return(list(
    mae_obs = crps_climate,
    crps_ens = crps_ensembles,
    crpss_climatology = crpss))
  
}

y_scores <- function(data_fore,data_input) {
  #volume data
  y_train = data_input$y_train$volume %>% as.matrix()
  #ensemble monthly average
  y_ens_cv_avg = apply(data_fore$y_ens_cv,MARGIN = 2,mean) %>% as.numeric()
  y_ens = t(data_fore$y_ens_cv) %>% as.matrix()

  uni_scores = deterministic_scores(y_true = y_train,y_pred = y_ens_cv_avg)
  ens_scores = ensemble_scores(y_train = y_train, y_ens = y_ens)
  
  scores = data.frame( c(uni_scores,ens_scores))
  return(scores)
}

q_scores <- function(q_fore,data_input) {
  #streamflow data
  q_train=data$q_train
  
  #ensemble monthly scores
  q_ens = q_fore
 
  uni_scores = deterministic_scores(y_true = q_train,y_pred = q_ens_cv_avg)
  ens_scores = ensemble_scores(y_train = q_train, y_ens = q_ens)
  
  scores = data.frame( c(uni_scores,ens_scores))
  return(scores)
}
