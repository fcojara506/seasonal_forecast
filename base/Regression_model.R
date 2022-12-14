set.seed(10)
library(truncnorm)
############## RMSE IN LEAVE.ONE.OUT #######################
rmse_LOO <- function(sim,obs) {
  
  require(hydroGOF)
  
  len_obs = length(obs)
  len_sim = length(sim)
  
  rmse_ <- matrix(nrow = len_sim)
  
  if(len_sim==len_obs){
    for (iterator in seq_along(sim)) {
      rmse_[iterator,]= hydroGOF::rmse(sim=sim[-iterator],obs=obs[-iterator])
    }
  }else{ stop("Size of y_sim is NOT equal to y_obs in RMSE")}
  return(rmse_)
}

#library(leaps)
# best_reg = regsubsets(
#   X_train,
#   y_train,
#   nvmax = NULL,
#   force.in = NULL,
#   force.out = NULL,
#   method = "exhaustive"
# )
# summary_best_subset <- summary(best_reg)
# summary_best_subset$which[which.max(summary_best_subset$adjr2),]

# linear regression
forecast_vol_determinist <- function(X_train,
                                     y_train,
                                     X_test,
                                     ...
                                     ) {
  
  library(caret)
  
  regression_model = train(
    X_train,
    y_train,
    metric = "RMSE",
    trControl  = 
      trainControl(
        method = "LOOCV",
        savePredictions = "all"
                 ),
    #method = method,
    #preProcess = preProcess
  ...
  )
  
  ## CROSS VALIDATION
  y_best_model_pred = merge(
    regression_model$bestTune,
    regression_model$pred) %>%
    arrange(rowIndex)
  
  y_cv                <- y_best_model_pred$pred
  
  # prediction for test data
  y_fore              <- predict(regression_model,
                                 newdata =X_test)[[1]]
  
  # errors
  rmse_cv              <- rmse_LOO(sim = y_cv,obs = y_train)
  rmse_model           <- hydroGOF::rmse(sim = y_cv, obs = y_train)
  
 return(list(y_cv = y_cv,
             y_fore = y_fore,
             rmse_cv = rmse_cv,
             rmse_model = rmse_model,
             regression_model = regression_model
             ))
}



############## ENSEMBLE GENERATOR

ensemble_generator <- function(y,rmse,n_members=1000){
  
  # GENERATE RANDOM PERTURBATION BASED ON THE RMSE IN CV FOR EACH YEAR
  ensemble_vol           <- matrix(nrow=n_members,ncol=length(rmse))
  
  for (i_year in seq_along(rmse)) {
    center = y[i_year]
    variation = rmse[i_year]
    # volume ensemble = center +- variation
   
    
    ensemble_vol[,i_year] <-  rtruncnorm(n = n_members,
                                         mean = center,
                                         sd = variation,
                                         a = 0,
                                         b = Inf) %>%
      matrix(n_members,1)
  }  

  return(ensemble_vol)
}


ensemble_post <- function(vol_det,data_input, n_members) {
  
  vol_det_unique = vol_det[[1]]
  # historical period
  y_ens_cv = ensemble_generator(y = vol_det_unique$y_cv,
                                rmse = vol_det_unique$rmse_cv,
                                n_members = n_members)
  
  colnames(y_ens_cv) = data_input$wy_train
  # prediction period
  y_ens_fore = ensemble_generator(y = vol_det_unique$y_fore,
                                  rmse = vol_det_unique$rmse_model,
                                  n_members = n_members)
  
  colnames(y_ens_fore) = data_input$wy_holdout
  
  return(
  c(list(
    y_ens_cv = y_ens_cv,
    y_ens_fore = y_ens_fore),
    purrr::transpose(vol_det)
  )
  )
}

ensemble_pre <- function(vol_det,data_input) {
  
  vol_det = purrr::transpose(vol_det)
  # historical period
  y_ens_cv = data.frame(vol_det$y_cv,check.names = F) %>% t
  colnames(y_ens_cv) = data_input$wy_train
  
  # prediction period
  y_ens_fore = data.frame(vol_det$y_fore,check.names = F) %>% t %>%  as.matrix()
  colnames(y_ens_fore) = data_input$wy_holdout
  
  
  return(
    c(list(
      y_ens_cv = y_ens_cv,
      y_ens_fore = y_ens_fore),
      vol_det
    )
  )
}

forecast_vol_ensemble <- function(data_input,
                                  n_members=1000,
                                  method='lm',
                                  preProcess = c("center", "scale"),
                                  ...
                                  ){
  # get ensemble names 
  ensemble_names = names(data_input$X_train)
  
  #initialise volume_regression vector for each ensemble
  vol_det = vector(mode = "list",length = length(ensemble_names))
  names(vol_det) = ensemble_names
  
  for (ens_i in ensemble_names) {
      #print(ens_i)
    
      X_train = data_input$X_train[[ens_i]]
      y_train = data_input$y_train$volume
      X_test  = data_input$X_test[[ens_i]] 
      
      vol_det[[ens_i]] =
      forecast_vol_determinist(
        X_train,
        y_train,
        X_test,
        method = method,
        preProcess = preProcess,
        ...
      )
    
  }
  
  if (length(ensemble_names)==1) {
    y_forecast = ensemble_post(vol_det, data_input,n_members)
  }else{
    
    y_forecast = ensemble_pre(vol_det,data_input)
    }
  
  
  return(y_forecast)
}


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
  
  obs_climate               <- rep(mean(y_train),times = nrow(y_train)) %>% 
    as.matrix()
  
  crps_ensembles            <- mean(SpecsVerification::EnsCrps(
    ens=y_ens,
    obs=y_train,
    R.new=NA)
    ) 
  
  crps_climate              <- hydroGOF::mae(sim = obs_climate,obs = y_train)
  crpss                     <- 1 - crps_ensembles/crps_climate
  
  return(list(
    mae_obs = crps_climate,
    crps_ens = crps_ensembles,
    crpss_climatology = crpss))
  
}

y_scores <- function(data_fore,data_input) {
  y_train = data_input$y_train$volume %>% as.matrix()
  y_ens_cv_avg = apply(data_fore$y_ens_cv,MARGIN = 2,mean) %>% as.numeric()
  y_ens = t(data_fore$y_ens_cv) %>% as.matrix()
  
  uni_scores = deterministic_scores(y_true = y_train,y_pred = y_ens_cv_avg)
  ens_scores = ensemble_scores(y_train = y_train, y_ens = y_ens)
  
  scores = data.frame( c(uni_scores,ens_scores))
  return(scores)
}










