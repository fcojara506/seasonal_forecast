rm(list = ls())
gc()
library(ggplot2)
library(curl)
library(dplyr)
library(data.table)
library(ncdf4)
library(lubridate)
source("W:/Mi unidad/CORFO_Maule_FJ/Pronostico_estacional/1.x_WYtime_functions.R")

##################################################################
##          VARIABLES DE ALMACENAMIENTO EN MODELO GR6J          ##
##################################################################

setwd("W:/Mi unidad/CORFO_Maule_FJ/Pronostico_estacional")
storage_vars_gr6j_daily<-readRDS("data_input/SimIHC-Q_wys1981-2020_forzERA5BC.RData") %>%
  rbindlist()%>%
  mutate(yr    = year(date)) %>%
  data.table() %>%
  setkey("yr") %>%
  wy_firstday_list[.] %>%
  .[,yr:=wy(date,first.day)] %>%
  .[,wy:=yr] %>%
  setkey("yr") %>%
  wy_firstday_list[.] %>%
  .[,woy:=wy_woy(date,first.day)] %>%
  .[,first.day:=NULL]%>%
  .[,i.first.day:=NULL]%>%
  .[,yr:=NULL]%>%
  .[,wym:=wym_simple(date)]%>%
  .[,wy_simple:=wy_simple(date)]%>%
  setkey("cuenca","date")

storage_vars_gr6j_monthly <- storage_vars_gr6j_daily %>%
  group_by(wy_simple,wym,cuenca) %>%
  summarise(SP   = tail(SP,1),
            PROD = tail(PROD,1),
            ROUT = tail(ROUT,1),
            EXP  = tail(EXP,1)
  )
feather::write_feather(storage_vars_gr6j_monthly,
                       paste0("data_input/variables_almacenamiento_gr6j_mensual.feather"))

# storage_vars_gr6j_weekly <- storage_vars_gr6j_daily %>%
#   group_by(wy,woy,cuenca) %>%
#   summarise(SP   = tail(SP,1),
#             PROD = tail(PROD,1),
#             ROUT = tail(ROUT,1),
#             EXP  = tail(EXP,1)
#             )
# feather::write_feather(storage_vars_gr6j_weekly,
#                        paste0("data_input/variables_almacenamiento_gr6j_semanal.feather"))

message(paste0("Última fecha disponible GRxJ (Y-M-D):",max(storage_vars_gr6j_daily$date)))
