rm(list = ls())
gc()

library(ggplot2)
library(curl)
library(dplyr)
library(data.table)
library(ncdf4)
library(lubridate)
library(pbapply)

setwd("~/GoogleDrive/CORFO_Maule_FJ/Pronostico_estacional")

starting_wy       <- 1988
ending_wy         <- 2021
cuencas_target    <- c("Achibueno","Ancoa","Melado","Maule","Lontue","Longavi")

cuenca_target="Achibueno"
get_predictors_basin <- function(cuenca_target,starting_wy,ending_wy) {
  message("Dando formato a ",cuenca_target)
##################################################################
##                   FORZANTES METEOROLOGICAS                   ##
################################################################## 

era5_monthly       <- feather::read_feather("data_input/forzantes_mensuales_1979_2021_era5corregido.feather") %>%
  subset(wy_simple>=starting_wy & wy_simple<=ending_wy & cuenca==cuenca_target) %>% data.table(key=c("wy_simple","wym","cuenca"))
#era_sofar_df <- era5_weekly %>% group_by(wy,cuenca,GRP) %>% summarise(tem = mean(tem),
#                                                                         pr  = sum(pr))
# ##################################################################
# ##                    INDICES DE GRAN ESCALA                   ##
# ##################################################################
# 
# indices_monthly    <- feather::read_feather("data_input/indices_mensuales_1988_2020.feather")%>%
#   subset(wy_simple>=starting_wy & wy_simple<=ending_wy)%>% data.table(key=c("wy_simple","wym"))
# #indices_sofar_df <- indices_weekly %>% group_by(wy,GRP) %>% summarise( nino1.2=mean(nino1.2),
# #                                                                       nino3   = mean(nino3),
# #                                                                       nino3.4 = mean(nino3.4),
# #                                                                       nino4   = mean(nino4),
# #                                                                       sswp    = mean(sswp))
##################################################################
##          VARIABLES DE ALMACENAMIENTO EN MODELO GR6J          ##
##################################################################
vars_gr6j_monthly  <- feather::read_feather("data_input/variables_almacenamiento_gr6j_mensual.feather")%>%
  subset(wy_simple>=starting_wy & wy_simple<=ending_wy & cuenca==cuenca_target)%>% data.table(key=c("wy_simple","wym","cuenca"))

predictores=era5_monthly %>%
  vars_gr6j_monthly[.] %>%
  #indices_monthly[.] %>%
  data.table() %>%
  setkey("wy_simple","wym")

return(predictores)
}


seasonal_volume_basin<- function(cuenca_target,starting_wy,ending_wy) {
  
  ##################################################################
  ##                VOLUMEN ESTACIONAL MILLONES M3                ##
  ##################################################################
  vol_estacional    <- feather::read_feather("data_input/vol_estacional_1978_2020.feather")%>%
    reshape2::melt(id.vars=c("wy","GRP")) %>%
    rename(cuenca=variable,volumen=value) %>%
    subset(wy>=starting_wy & wy<=ending_wy & cuenca==cuenca_target)
  
}

#MAIN

volumenes   <- pblapply(cuencas_target,function(x) seasonal_volume_basin(x,starting_wy,ending_wy)) %>% rbindlist
feather::write_feather(volumenes,paste0("data_input/MODELO_VOLUMENES_ESTACIONALES.feather"))


predictores <- pblapply(cuencas_target,function(x) get_predictors_basin(x,starting_wy,ending_wy)) %>% rbindlist
feather::write_feather(predictores,paste0("data_input/MODELO_PREDICTORES_MENSUALES.feather"))
