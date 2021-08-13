# FREE ENVIRONMENT FILES
rm(list = ls())
gc()

# LOAD LIBRARIES 
load_silently <- function(lib_name) {suppressWarnings(suppressMessages(require(lib_name, character.only = TRUE)))}
sapply(c("feather","ggplot2","dplyr","data.table","pls","hydroGOF"),load_silently)

########## SET CURRENT DIRECTORY ###########################
setwd("~/GoogleDrive/CORFO_Maule_FJ/Pronostico_estacional")

############## RMSE IN LEAVE.ONE.OUT #######################
rmse_LOO <- function(sim,obs) {
  require(hydroGOF)
  if(length(sim)==length(obs)){
    rmse_=sapply(seq_along(sim),function(iterator) rmse(sim=sim[-iterator],obs=obs[-iterator]))
    rmse_=append(rmse_,rmse(sim,obs))
  }else{ message("Size of sim is NOT equal to obs's"); rmse_=c()}
  return(rmse_)
}
############## ENSEMBLE GENERATOR

emsemble_volume2 <- function(ensemble_members=1000,
                             predicted_volumes,
                             RMSE_LOOCV,
                             wy_array,
                             predictores_col,
                             periodo_volumen) {
  
  # GENERATE RANDOM PERTURBATION BASED ON THE RMSE IN CV FOR EACH YEAR
  vol.ens=matrix(nrow=ensemble_members,ncol=length(RMSE_LOOCV))
  for (i_year in seq_along(RMSE_LOOCV)) {
    vol.ens[,i_year] <- rnorm(ensemble_members,mean=0,sd=RMSE_LOOCV[i_year]) %>%
      matrix(ensemble_members,1) %>%
      sweep(MARGIN = 2, predicted_volumes[i_year], `+`)
  }          
  # ADAPT TO EXPORT RESULTS
  vol.ens.export    = vol.ens %>%
    data.frame  %>%
    `colnames<-` (wy_array) %>%
    reshape2::melt(id.vars        = NULL,
                   variable.name  = "wy",
                   value.name     = "volumen") %>%
    mutate(version     = paste(as.character(predictores_col), collapse = " + "),
           wym         = wym_inicio,
           cuenca      = cuenca_target,
           periodo     = periodo_volumen,
           predictando = tipo_predictando)
  
  return(vol.ens.export)
}

############## STATISTICAL MODEL

modelo_estadistico_v2 <- function(wym_inicio,wy_target,tipo_predictando,cuenca_target,predictores_col,exportar_predictores=F) {
  
  # SEQUENCE OF MONTHS TO PREDICT (DEFAULT 6-12: SEP-MAR, DYNAMIC AFTER SEPTEMBER)
  periodo_volumen       <- ifelse(tipo_predictando=="predictando_estatico",
                                  "[6-12]", #FIJO SEP-MAR
                                  paste0("[",ifelse(wym_inicio>5,wym_inicio+1,6),"-12]")) # MOVIL ENTRE SEP-MAR
  paste0(" Basin:", cuenca_target,
         "____ Month:", wym_inicio,
         "____ id_predictores:",paste(as.character(predictores_col), collapse = " + "),
         "____ Periodo:",periodo_volumen,
         "____ Predictando: ", tipo_predictando) %>% 
    message
  
  #LOAD AND MODIFY MONTHLY PREDICTORS. EACH PREDICTOR HAS ONLY ONE VALUE A YEAR
  # CLIMATOLOGICAL PREDICTORS: MEAN TEMPERATURE AND CUMMULATIVE PRECIPITATION FROM APRIL TO CURRENT MONTH,
  # HYDROLOGICAL PREDICTORS: LAST SIMULATED STATE OF THE HYDROLOGICAL MODEL'S STORAGES
  
  predictores_GRP      <-read_feather("data_input/MODELO_PREDICTORES_MENSUALES.feather") %>%
    subset(cuenca == cuenca_target) %>% 
    subset(wym %in% seq(1,wym_inicio))%>%
    group_by(wy_simple) %>%
    summarise(
      tem_mean   = mean(tem),
      pr_sum     = sum(pr),
      CI_last    = tail(SP, 1)+tail(PROD,1) + tail(ROUT,1)# - tail(EXP,1),
    )
  
  # LOAD SEASONAL VOLUME FOR EACH YEAR  
  volumenes             <- read_feather("data_input/MODELO_VOLUMENES_ESTACIONALES.feather")
  
  #################################################################
  ##                         TRANING                             ##
  #################################################################
  
  # TRAINING PERIOD -PREDICTORS (ALL THE PERIOD EXCEPT TARGET YEAR'S)
  predictores_GRP_train <- predictores_GRP %>%
    subset(wy_simple!=wy_target)
  
  # TRAINING PERIOD : VOLUMES
  volumenes_train       <- volumenes %>%
    subset(cuenca == cuenca_target) %>%
    subset(GRP == periodo_volumen & wy!=wy_target) %>%
    rename(wy_simple=wy) %>%
    select(-GRP) 
  
  # TRAINING HYDROLOGICAL YEARS
  wy_train              <- unique(volumenes_train$wy_simple)
  
  # TRAINING ANNUAL BASIN'S PREDICTORS AND VOLUME 
  data_train_cuenca  <- merge.data.frame(predictores_GRP_train, volumenes_train,by=c("wy_simple")) %>% 
    structure(row.names = wy_train) %>%
    select(-cuenca,-wy_simple)%>%
    select(predictores_col %>% c("volumen"))
  
  #################################################################
  ##                  PREDICTING TARGET YEAR                     ##
  #################################################################
  
  # TARGET YEAR'S PREDICTORS
  data_pred_cuenca <- predictores_GRP %>%
    subset(wy_simple %in% wy_target) %>%
    transform(wy_simple=NULL) %>%
    select(predictores_col)
  
  ##################################################################
  ##               PARTIAL LEAST-SQUARES REGRESSION               ##
  ##################################################################
  
  # FIT MODEL USING TRAINING DATA
  plsr.fit           <- plsr(
    volumen ~ .,
    data = data_train_cuenca,
    scale = T,
    ncomp = 1,
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
  
  ## GENERATE ENSEMBLE BASED ON RMSE AND THE PREDICTED VOLUME
  vol.ens.exp= emsemble_volume2(ensemble_members = 1000,
                                predicted_volumes = predicted_volumes,
                                RMSE_LOOCV        = rmse_loocv,
                                wy_array          = c(wy_train,wy_target),
                                predictores_col   = predictores_col,
                                periodo_volumen   = periodo_volumen)
  
  return(vol.ens.exp)
}

##### INPUTS ########

wyms_inicio        <- 4 #1=apr,2=may,3=jun,4=jul,5=ago,6=sep,7=oct,8=nov,9=dic,10=ene,11=feb,12=mar
wy_target          <- 2021
tipos_predictando  <- c("predictando_dinamico")
cuencas_target     <- c("Achibueno", "Maule","Lontue","Longavi")
predictores_cols   <- list(c("pr_sum","tem_mean"))#list( c("CI_last"), c("pr_sum","tem_mean"),c("CI_last","pr_sum","tem_mean"))

resultados = list()
w          = 1
for (wym_inicio in wyms_inicio) {
  for (tipo_predictando in tipos_predictando) {
    for (cuenca_target in cuencas_target) {
      for (predictores_col in predictores_cols) {
        resultados[[w]]=modelo_estadistico_v2(wym_inicio = wym_inicio,
                                              wy_target = wy_target,
                                              tipo_predictando=tipo_predictando,
                                              cuenca_target=cuenca_target,
                                              predictores_col = predictores_col,
                                              exportar_predictores = T)
        w=w+1
      }
    }
  }
}


#EXPORT PREDICTED VOLUMES
resultados %>%
  rbindlist %>%
  feather::write_feather(paste0("data_output/pronostico_vol_estacional_v2.feather"))
