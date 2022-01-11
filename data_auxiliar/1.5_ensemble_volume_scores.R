############ VERIFICATION SCORES
#predicted_vol_ensembles<-vol.ens.export
#vol.obs
verification_scores <- function(predicted_vol_ensembles,vol.obs) {
  require(hydroGOF)
  
  vol_obs_pred=predicted_vol_ensembles %>%
    select(wy,volumen) %>% 
    transform(wy=as.numeric(as.character(wy))) %>% 
    data.table(key="wy") %>% 
    .[,ens := seq(1,.N),by="wy"] %>% 
    data.table::dcast(wy~ens,value.var = "volumen") %>% 
    merge(vol.obs,.,by.x=c("wy_simple"),by.y="wy") %>% 
    select(-cuenca,-wy_simple)
  
  #ensemble scores
  obs                       <- select(vol_obs_pred,volumen) %>%  as.matrix() # observation vector
  obs_climate               <- obs %>% transform(mean=mean(volumen)) %>% select(mean)%>%  as.matrix() 
  
  eps                       <- select(vol_obs_pred,-volumen)%>%  as.matrix()# prediction matrix
  
  crps_ensembles            <- mean(SpecsVerification::EnsCrps(ens=eps,obs=obs,R.new=NA)) # ensemble crps
  crps_climate              <- hydroGOF::mae(obs_climate,obs)#mean(abs(obs - mean(obs)))        # climate reference
  crpss                     <- 1 - crps_ensembles/crps_climate 
  
  #deterministic scores
  pred_deterministic        <- apply(eps, 1, mean) %>%  as.matrix()# prediction vector deterministic mean
  scores_deterministic      <- hydroGOF::gof.data.frame(sim=pred_deterministic,obs = obs)
  
  export_list               <- data.frame(crps_ensembles  = crps_ensembles,
                                    crpss           = round(crpss,3),
                                    rmse            = scores_deterministic["RMSE",][[1]],
                                    R2              = scores_deterministic["R2",][[1]]
  )
  return(export_list)
}

  
############## RMSE IN LEAVE.ONE.OUT #######################
rmse_LOO <- function(sim,obs) {
  require(hydroGOF)
  if(length(sim)==length(obs)){
    rmse_=sapply(seq_along(sim),function(iterator) rmse(sim=sim[-iterator],obs=obs[-iterator]))
    rmse_=append(rmse_,rmse(sim,obs))
  }else{ message("Size of sim is NOT equal to obs's"); rmse_=c()}
  return(rmse_)
}


