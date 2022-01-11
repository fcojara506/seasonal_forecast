############## ENSEMBLE GENERATOR

emsemble_volume2 <- function(ensemble_members=10000,
                             predicted_volumes,
                             RMSE_LOOCV,
                             wy_array,
                             predictores_col,
                             periodo_volumen,
                             vol.obs,
                             regression_type) {
  
  # GENERATE RANDOM PERTURBATION BASED ON THE RMSE IN CV FOR EACH YEAR
  vol.ens           <- matrix(nrow=ensemble_members,ncol=length(RMSE_LOOCV))
  
  
  for (i_year in seq_along(RMSE_LOOCV)) {
    vol.ens[,i_year] <- rnorm(ensemble_members,mean=0,sd=RMSE_LOOCV[i_year]) %>%
      matrix(ensemble_members,1) %>%
      sweep(MARGIN = 2, predicted_volumes[i_year], `+`)
  }  
  
  
  # ADAPT TO EXPORT RESULTS
  vol.ens.export    <- vol.ens %>%
    data.frame  %>%
    `colnames<-` (wy_array) %>%
    reshape2::melt(id.vars        = NULL,
                   variable.name  = "wy",
                   value.name     = "volumen")
  ############################################################################## transformar miembros en columnas
  scores=verification_scores(vol.ens.export,vol.obs)
  
 
  vol.ens.export = vol.ens.export %>% #data.frame(volumen=NA, wy=NA) %>% #vol.ens.export %>%
    mutate(scores) %>% 
    mutate(version          = paste(as.character(predictores_col), collapse = " / ")) %>% 
    mutate(wym              = wym_inicio) %>% 
    mutate(cuenca           = cuenca_target) %>% 
    mutate(periodo          = periodo_volumen) %>% 
    mutate(predictando      = tipo_predictando) %>% 
    mutate(regression_type  = regression_type)
  
  return(vol.ens.export)
}


##################################################################
##                LINEAR            REGRESSION               ##
##################################################################

linear_regression <- function(data_train_predict,tag="lineal") {
  
  # SPLIT THE LIST INTO TRAINING AND PREDICTION
  data_train_cuenca <- data_train_predict$data_train_cuenca
  data_pred_cuenca  <- data_train_predict$data_pred_cuenca
  
  ##################################################################
  ##               EXPLORING      REGRESSIONS                    ##
  ##################################################################
  
  library(caret)
  
  model <- train(volumen~ .,#poly(CI_last_v1,2),
                 data       = data_train_cuenca,
                 method     = "lm",
                 trControl  = trainControl(method = "cv",number = 41,savePredictions = T))
  
  
  ## CROSS VALIDATION'S RESULTS FOR TRAINING PERIOD
  obs.lm              <- model$trainingData$.outcome
  pred.lm             <- model$pred$pred %>% as.numeric() 
  rmse_loocv_lm       <- rmse_LOO(sim=pred.lm,obs=obs.lm)
  ## DETERMINISTIC PREDICTION FOR TARGET YEAR
  predict_volume_lm    <- model %>% predict(newdata = data_pred_cuenca) %>% as.numeric
  predicted_volumes_lm <- c(pred.lm,predict_volume_lm)
  
  ## volume observed
  vol.obs.ver=data.frame(wy_simple= rownames(data_train_cuenca),
                         cuenca   = data_train_predict$cuenca,
                         volumen  = data_train_cuenca$volumen)
  
  ## GENERATE ENSEMBLE BASED ON RMSE AND THE PREDICTED VOLUME
  vol.ens.exp= emsemble_volume2(ensemble_members  = 1000,
                                predicted_volumes = predicted_volumes_lm,
                                RMSE_LOOCV        = rmse_loocv_lm,
                                wy_array          = c(rownames(data_train_cuenca),rownames(data_pred_cuenca)),
                                predictores_col   = data_train_predict$predictores_col,
                                periodo_volumen   = data_train_predict$periodo_volumen,
                                vol.obs           = vol.obs.ver,
                                regression_type   = tag)
  
  return(vol.ens.exp)
}

log_regression <- function(data_train_predict,tag="log") {
  
  # SPLIT THE LIST INTO TRAINING AND PREDICTION
  data_train_cuenca <- data_train_predict$data_train_cuenca %>% 
    transform(volumen=log(volumen))
  data_pred_cuenca  <- data_train_predict$data_pred_cuenca
  
  ##################################################################
  ##               EXPLORING      REGRESSIONS                    ##
  ##################################################################
  
  library(caret)
  
  model <- train(volumen~ .,#poly(CI_last_v1,2),
                 data       = data_train_cuenca,
                 method     = "lm",
                 trControl  = trainControl(method = "cv",number = 41,savePredictions = T))
  
  
  ## CROSS VALIDATION'S RESULTS FOR TRAINING PERIOD
  obs.lm              <- model$trainingData$.outcome %>% exp()
  pred.lm             <- model$pred$pred %>% as.numeric() %>% exp()
  rmse_loocv_lm       <- rmse_LOO(sim=pred.lm,obs=obs.lm)
  ## DETERMINISTIC PREDICTION FOR TARGET YEAR
  predict_volume_lm    <- model %>% predict(newdata = data_pred_cuenca) %>% as.numeric %>% exp()
  predicted_volumes_lm <- c(pred.lm,predict_volume_lm)
  
  ## OBSERVED VOLUME
  vol.obs.ver=data.frame(wy_simple= rownames(data_train_cuenca),
                         cuenca   = data_train_predict$cuenca,
                         volumen  = data_train_cuenca$volumen)
  
  ## GENERATE ENSEMBLE BASED ON RMSE AND THE PREDICTED VOLUME
  vol.ens.exp= emsemble_volume2(ensemble_members  = 1000,
                                predicted_volumes = predicted_volumes_lm,
                                RMSE_LOOCV        = rmse_loocv_lm,
                                wy_array          = c(rownames(data_train_cuenca),rownames(data_pred_cuenca)),
                                predictores_col   = data_train_predict$predictores_col,
                                periodo_volumen   = data_train_predict$periodo_volumen,
                                vol.obs           = vol.obs.ver,
                                regression_type   = tag)
  
  return(vol.ens.exp)
}


##################################################################
##               PARTIAL LEAST-SQUARES REGRESSION               ##
##################################################################

plsr_regression<- function(data_train_predict,tag="PLSR") {
  
  # SPLIT THE LIST INTO TRAINING AND PREDICTION
  data_train_cuenca <- data_train_predict$data_train_cuenca
  data_pred_cuenca  <- data_train_predict$data_pred_cuenca
  
  
  # FIT MODEL USING TRAINING DATA
  plsr.fit           <- plsr(
    volumen ~ .,
    data = data_train_cuenca,
    scale = F,
    ncomp =1,
    #length.seg= 3,
    #segment.type="consecutive",
    validation = "LOO",
    method = "simpls",
    jackknife = TRUE)
  
  ## CROSS VALIDATION'S RESULTS FOR TRAINING PERIOD
  obs.plsr           <- plsr.fit$model$volumen
  pred.plsr          <- plsr.fit$validation$pred %>% as.numeric()
  rmse_loocv         <- rmse_LOO(sim=pred.plsr,obs=obs.plsr)
  
  ## DETERMINISTIC PREDICTION FOR TARGET YEAR
  predict_volume     <- plsr.fit %>% predict(newdata = data_pred_cuenca, ncomp = 1) %>% as.numeric
  predicted_volumes  <- c(pred.plsr,predict_volume)
  
  ## OBSERVED VOLUME
  vol.obs.ver=data.frame(wy_simple= rownames(data_train_cuenca),
                         cuenca   = data_train_predict$cuenca,
                         volumen  = data_train_cuenca$volumen)
  
  ## GENERATE ENSEMBLE BASED ON RMSE AND THE PREDICTED VOLUME
  vol.ens.exp= emsemble_volume2(ensemble_members  = 1000,
                                predicted_volumes = predicted_volumes_lm,
                                RMSE_LOOCV        = rmse_loocv_lm,
                                wy_array          = c(rownames(data_train_cuenca),rownames(data_pred_cuenca)),
                                predictores_col   = data_train_predict$predictores_col,
                                periodo_volumen   = data_train_predict$periodo_volumen,
                                vol.obs           = vol.obs.ver,
                                regression_type   = tag)
}


loc_regression  <- function(data_train_predict,degree=1,alpha_min=0.1,tag = "") {
  
  # SPLIT THE LIST INTO TRAINING AND PREDICTION
  data_train_cuenca <- data_train_predict$data_train_cuenca
  
  ##################################################################
  ##               EXPLORING      REGRESSIONS                    ##
  ##################################################################
  loc.wrapper <- function(data_train_cuenca,alpha_min,degree){
    
    library(locfit)

    # formula_locfit <- function(data_train_cuenca){
    #   as.formula(paste0("volumen~ lp(",
    #                      paste(colnames(data_train_cuenca %>% select(-volumen)) , collapse = ","),
    #                      ",scale=T,nn=",span,",deg=",degree,")"
    #   ))}
    
    #plot(data_train_cuenca$SP,data_train_cuenca$volumen)
    
    alpha_grid        = seq(alpha_min,1,by=0.01)
    best_alpha_by_gcv = gcvplot(volumen~.,data=data_train_cuenca,maxk=10000,alpha = alpha_grid,deg=degree)$value %>% order %>% head(1) %>% alpha_grid[.]
    
    #a=locfit(volumen~SP,data=data_train_cuenca,maxk=5000,alpha = best_alpha_by_gcv)
    #plot(a,get.data = T)
    #predict(a,newdata = data.frame(SP=250))
    
    theta.fit     <- function(data_train_cuenca) locfit(volumen~.,data=data_train_cuenca,maxk=5000,alpha = best_alpha_by_gcv,deg=degree)
    theta.predict <- function(fitted_model, x0) predict(fitted_model, newdata=x0,type='response')
    
    y.cv <- fco_cv_locfit(data_train_cuenca, theta.fit, theta.predict)
    
    y.cv$model=theta.fit(data_train_cuenca)
    y.cv$best_alpha_by_gcv=best_alpha_by_gcv
    return(y.cv)
  }
  
  
  model=loc.wrapper(data_train_cuenca,alpha_min = alpha_min,degree = degree)
  ## CROSS VALIDATION'S RESULTS FOR TRAINING PERIOD
  obs.lm              <- model$training_data$predictand %>% as.numeric()
  pred.lm             <- model$cv.fit
  
  rmse_loocv_lm       <- rmse_LOO(sim=pred.lm,obs=obs.lm)
  ## DETERMINISTIC PREDICTION FOR TARGET YEAR
  data_pred_cuenca  <- data_train_predict$data_pred_cuenca %>% as.matrix()
  
  predict_volume_lm    <- model$model %>% predict(newdata = data_pred_cuenca) %>% as.numeric
  predicted_volumes_lm <- c(pred.lm,predict_volume_lm)
  
  ## OBSERVED VOLUME
  vol.obs.ver=data.frame(wy_simple= rownames(data_train_cuenca),
                         cuenca   = data_train_predict$cuenca,
                         volumen  = data_train_cuenca$volumen)
  
  ## GENERATE ENSEMBLE BASED ON RMSE AND THE PREDICTED VOLUME
  vol.ens.exp= emsemble_volume2(ensemble_members  = 1000,
                                predicted_volumes = predicted_volumes_lm,
                                RMSE_LOOCV        = rmse_loocv_lm,
                                wy_array          = c(rownames(data_train_cuenca),rownames(data_pred_cuenca)),
                                predictores_col   = data_train_predict$predictores_col,
                                periodo_volumen   = data_train_predict$periodo_volumen,
                                vol.obs           = vol.obs.ver,
                                regression_type   = paste0("locfit:deg",degree,"_alpha_min:",alpha_min)
  )
  
  return(vol.ens.exp)
}


loess_regression <- function(data_train_predict,span=0.5,tag="loess") {
  #stop()
  # SPLIT THE LIST INTO TRAINING AND PREDICTION
  data_train_cuenca <- data_train_predict$data_train_cuenca
  
  
  
  ##################################################################
  ##               EXPLORING      REGRESSIONS                    ##
  ##################################################################
  loess.wrapper <- function(data_train_cuenca,span){
    x=data_train_cuenca %>% select(-volumen)
    y=data_train_cuenca %>% select(volumen) 
    kfold=nrow(y)
    
    theta.fit   <- function(x,y, span) loess(y ~ x, span=span,control = loess.control(surface = "direct"))
    theta.predict <- function(fit, x0) predict(fit, newdata=x0)
    y.cv <- fco_cv(x, y, theta.fit, theta.predict, span=span, ngroup = kfold)
    y.cv$model=theta.fit(as.matrix(x),as.matrix(y),span)
    return(y.cv)
  }
  
  
  model=loess.wrapper(data_train_cuenca,span)
  ## CROSS VALIDATION'S RESULTS FOR TRAINING PERIOD
  obs.lm              <- model$training_data$predictand %>% as.numeric()
  pred.lm             <- model$cv.fit
  rmse_loocv_lm       <- rmse_LOO(sim=pred.lm,obs=obs.lm)
  ## DETERMINISTIC PREDICTION FOR TARGET YEAR
  data_pred_cuenca  <- data_train_predict$data_pred_cuenca %>% as.matrix()
  
  predict_volume_lm    <- model$model %>% predict(newdata = data_pred_cuenca) %>% as.numeric
  predicted_volumes_lm <- c(pred.lm,predict_volume_lm)
  
  ## OBSERVED VOLUME
  vol.obs.ver=data.frame(wy_simple= rownames(data_train_cuenca),
                         cuenca   = data_train_predict$cuenca,
                         volumen  = data_train_cuenca$volumen)
  
  ## GENERATE ENSEMBLE BASED ON RMSE AND THE PREDICTED VOLUME
  vol.ens.exp= emsemble_volume2(ensemble_members  = 1000,
                                predicted_volumes = predicted_volumes_lm,
                                RMSE_LOOCV        = rmse_loocv_lm,
                                wy_array          = c(rownames(data_train_cuenca),rownames(data_pred_cuenca)),
                                predictores_col   = data_train_predict$predictores_col,
                                periodo_volumen   = data_train_predict$periodo_volumen,
                                vol.obs           = vol.obs.ver,
                                regression_type   = tag)
  
  return(vol.ens.exp)
}
