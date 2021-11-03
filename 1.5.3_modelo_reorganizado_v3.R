# FREE ENVIRONMENT FILES
rm(list = ls())
gc()
set.seed(2)
# LOAD LIBRARIES 
load_silently <- function(lib_name) {suppressWarnings(suppressMessages(require(lib_name, character.only = TRUE)))}
sapply(c("feather","ggplot2","dplyr","data.table","pls","hydroGOF"),load_silently)

########## SET CURRENT DIRECTORY ###########################
setwd("~/GoogleDrive/CORFO_Maule_FJ/Pronostico_estacional")

source("data_auxiliar/cross_validation.R")
source("data_auxiliar/1.5_regression_models.R")
source("data_auxiliar/1.5_ensemble_volume_scores.R")

############## STATISTICAL MODEL

split_train_predict <- function(wym_inicio,wy_target,tipo_predictando,cuenca_target,predictores_col,exportar_predictores=F) {
  
  # SEQUENCE OF MONTHS TO PREDICT (DEFAULT 6-12: SEP-MAR, DYNAMIC AFTER SEPTEMBER)
  periodo_volumen       <- ifelse(tipo_predictando=="predictando_estatico",
                                  "[6-12]", #FIJO SEP-MAR
                                  paste0("[",ifelse(wym_inicio>5,wym_inicio+1,6),"-12]")) # MOVIL ENTRE SEP-MAR
  
  paste0(" Basin:", cuenca_target,
         "____ Month:", wym_inicio,
         "____ id_predictores:",paste(as.character(predictores_col), collapse = " / "),
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
      
      tem_mean_3mons = mean(tail(tem,3)),
      
      pr_acum        = sum(pr),
      pr_acum_3mons  = sum(tail(pr,3)),
      # last value of each month
      CI             = tail(SP, 1) + tail(PROD,1) + tail(ROUT,1),
      
      # last value of each month
      SWE            = tail(SP,1),
      PROD           = tail(PROD,1),
      ROUT           = tail(ROUT,1),
      
      # rolling average 1 months
      PROD_avg1m     = tail(PROD_mean,1),
      SWE_avg1m       = tail(SP_mean,1),
      ROUT_avg1m     = tail(ROUT_mean,1),
      CI_avg1m       = SWE_avg1m+PROD_avg1m+ROUT_avg1m,
      
      # rolling average 2 months
      PROD_avg2m     = mean(tail(PROD_mean,2)),
      SWE_avg2m       = mean(tail(SP_mean,2)),
      
      # rolling average 3 months
      PROD_avg3m     = mean(tail(PROD_mean,3)),
      SWE_avg3m       = mean(tail(SP_mean,3)),
      ROUT_avg3m     = mean(tail(ROUT_mean,3)),
      
      # average of the entire period  
      SWE_mean_period   = mean(SP_mean),
      PROD_mean_period = mean(PROD_mean),
      CI_mean_period   = mean(SP)+mean(PROD)+mean(ROUT)
    )
  
  #################################################################
  ##                         TRAINING                             ##
  #################################################################
  
  # TRAINING PERIOD -PREDICTORS (ALL THE PERIOD EXCEPT TARGET YEAR'S)
  predictores_GRP_train <- predictores_GRP %>%
    subset(wy_simple!=wy_target)
  
  # TRAINING PERIOD : VOLUMES
  volumenes_train       <- volumenes %>%
    subset(cuenca == cuenca_target) %>%
    subset(GRP == periodo_volumen & wy!=wy_target) %>%
    rename(wy_simple=wy) %>%
    #subset(wy_simple != "1998") %>% 
    select(-GRP) %>%
    na.omit()
  
  # TRAINING HYDROLOGICAL YEARS
  wy_train              <- unique(volumenes_train$wy_simple)
  
  # TRAINING ANNUAL BASIN'S PREDICTORS AND VOLUME 
  data_train_cuenca  <- merge.data.frame(predictores_GRP_train, volumenes_train,by=c("wy_simple")) %>%  
    #subset(wy_simple != "1998") %>% 
    structure(row.names = wy_train) %>%
    select(-cuenca,-wy_simple) %>%
    select(all_of(predictores_col) %>% c("volumen"))
    
  
  #################################################################
  ##                  PREDICTING TARGET YEAR                     ##
  #################################################################
  
  # TARGET YEAR'S PREDICTORS
  data_pred_cuenca <- predictores_GRP %>%
    subset(wy_simple %in% wy_target) %>%
    transform(wy_simple=NULL) %>%
    select(all_of(predictores_col)) %>%
    structure(row.names = as.character(wy_target))
  
  data_export <-       list(data_pred_cuenca  = data_pred_cuenca,
                            data_train_cuenca = data_train_cuenca,
                            predictores_col   = predictores_col,
                            periodo_volumen   = periodo_volumen,
                            cuenca            = cuenca_target)
  return(data_export)
}


export_predictors <- function(data_train_predict) {
  
  data_train_cuenca <- data_train_predict$data_train_cuenca
  data_pred_cuenca  <- data_train_predict$data_pred_cuenca
  wy_train          <- rownames(data_train_cuenca)
  wy_target         <- rownames(data_pred_cuenca)
  
  exp.predictors  <- data_train_cuenca %>% 
    select(-volumen) %>% 
    transform(wy=wy_train) %>%
    rbind(transform(data_pred_cuenca,wy=wy_target)) %>% 
    transform(cuenca=cuenca_target, wym=wym_inicio) %>%
    reshape2::melt(id.var=c("wy","cuenca","wym"))
  
  return(exp.predictors)
}




##### INPUTS ########
# LOAD SEASONAL VOLUME FOR EACH YEAR  
volumenes          <- read_feather("data_input/MODELO_VOLUMENES_ESTACIONALES.feather")
wyms_inicio        <- seq(11,1) #1=apr,2=may,3=jun,4=jul,5=ago,6=sep,7=oct,8=nov,9=dic,10=ene,11=feb,12=mar
wy_target          <- 2021
tipos_predictando  <- "predictando_estatico" # dinamico: de sep a mar el vol es mes-marzo # estatico: vol es fijo sep-mar
cuencas_target     <- c("Maule","Melado","Achibueno","Lontue","Longavi","Ancoa")
predictores_cols   <- list(c("SWE","PROD","ROUT"),
                           c("pr_acum","tem_mean_3mons"),
                           #c("SWE","PROD"),
                           #c("SWE_mean_period","PROD_mean_period"),
                           #c("SWE_avg1m","PROD_avg1m"),
                           #c("SWE_avg2m","PROD_avg2m"),
                           #c("SWE_avg3m","PROD_avg3m"),
                           c("SWE_avg3m","PROD_avg3m","ROUT_avg3m")
                           #"pr_acum",
                           #"CI",
                           #"CI_mean_period",
                           #"SWE_mean_period",
                           #"PROD",
                           #"PROD_avg1m",
                           #"PROD_avg2m",
                           #"PROD_avg3m",
                           #"SWE",
                           #"SWE_mean_period",
                           #"SWE_avg1m",
                           #"SWE_avg2m",
                           #"SWE_avg3m",
                           #"ROUT",
                           #"CI_avg1m",
                           #"PROD_mean_period",
                           #"pr_acum_3mons"
                           ) #list(c("SWE","PROD","ROUT"),c("pr_sum"),"CI_last")

N=10^6
reg_lineal = vector(mode = "list", length = N)
reg_loc1   = vector(mode = "list", length = N)
reg_loc2   = vector(mode = "list", length = N)

resultados = vector(mode= "numeric",length = N)
list_scores=data_train_predict=resultados

predictors = vector(mode = "list", length = N)
w          = 1

for (wym_inicio in wyms_inicio) {
  for (tipo_predictando in tipos_predictando) {
    for (cuenca_target in cuencas_target) {
      for (predictores_col in predictores_cols) {
        
        data_train_predict <- split_train_predict(wym_inicio    = wym_inicio,
                                            wy_target           = wy_target,
                                            tipo_predictando    = tipo_predictando,
                                            cuenca_target       = cuenca_target,
                                            predictores_col     = predictores_col)
          
          #predictors[[w]]   <- export_predictors(data_train_predict)
          
         reg_lineal[[w]]    <- linear_regression(data_train_predict)
         reg_loc1[[w]]      <- loc_regression(data_train_predict,degree=1,alpha_min=0.2)

        w=w+1
      }
    }
  }
}

## ensemble volume dataframe
resultados=
  (reg_lineal %>% rbindlist) %>%
  rbind(reg_loc1 %>% rbindlist)

# ## scores from scenarios
 list_scores=resultados %>%
   select(-volumen,-wy) %>% 
   unique()
feather::write_feather(list_scores,path = "data_output/list_scores_seasonal_volume.feather")

#EXPORT PREDICTED VOLUMES
resultados= resultados %>% 
  select(wy,volumen,version,wym,cuenca,periodo,predictando,regression_type)
feather::write_feather(resultados,paste0("data_output/pronostico_vol_estacional_v2.feather"))

# EXPORT PREDICTORS

# predictors %>%
#   rbindlist %>%
#   unique() %>% 
#   data.table %>%
#   data.table::dcast(wy+cuenca+wym~variable) %>% 
#   feather::write_feather(paste0("data_output/predictores_v2.feather"))

#source("1.6.1.1_Comparacion_modelos_v1_v2.R")
#source("1.7.1_plotear_list_scores.R")
