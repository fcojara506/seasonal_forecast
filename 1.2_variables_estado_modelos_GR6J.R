rm(list = ls())
gc()


library(dplyr)
library(data.table)
library(lubridate)

##################################################################
##          VARIABLES DE ALMACENAMIENTO EN MODELO GR6J          ##
##################################################################

setwd("~/GoogleDrive/CORFO_Maule_FJ/Pronostico_estacional")

source("1.x_WYtime_functions.R")

storage_vars_gr6j_daily<-readRDS("data_input/SimIHC-Q_wys1981-2021_forzERA5BC.RData") %>% 
  rbindlist()%>%
  select(-wy) %>%
  transform(wy_simple=wy_simple(date)) %>% 
  transform(wym=wym_simple(date))


storage_vars_gr6j_monthly <-
  storage_vars_gr6j_daily %>%
  group_by(wy_simple,wym,cuenca) %>%
  summarise(SP_mean         = mean(SP),
            PROD_mean       = mean(PROD),
            ROUT_mean       = mean(ROUT),
            SP              = tail(SP,1),
            PROD            = tail(PROD,1),
            ROUT            = tail(ROUT,1),
            EXP             = tail(EXP,1)
  )

feather::write_feather(storage_vars_gr6j_monthly,
                       "data_input/variables_almacenamiento_gr6j_mensual.feather")

# a=subset(storage_vars_gr6j_monthly,cuenca=="Maule") %>% ungroup() %>%  select(-wy_simple, -cuenca,-wym)
# b=cor(a)
# corrplot::corrplot(b,method="number",type="lower")


