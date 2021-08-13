rm(list = ls())
gc()
library(ggplot2)
library(curl)
library(dplyr)
library(data.table)
library(ncdf4)
library(lubridate)
library(magrittr)

setwd("~/GoogleDrive/CORFO_Maule_FJ/Pronostico_estacional")

wym_simple       <- function(x){fifelse(month(x)>3, month(x)-3,month(x)+9)}
yr                <- function(mes,wyr){fifelse(as.numeric(as.character(mes))>3,
                                               as.numeric(wyr),as.numeric(wyr)+1)}
dayofmonth        <-function(mes,wyr){days_in_month(as.Date(paste0(yr(mes,wyr),"-",mes,"-01")))}
## load daily stream-flow time series (except MELADO monthly averaged)

Data_Qf_m3s=readRDS("data_input/Data_Qf_m3s_wys1978-2021.RData")
filename_Q_mensual_melado="data_input/Q_Melado_1960-2019_m3s-mes.RData"


##################################################################
##                  CAUDAL MEDIO DIARIO (m3/s)                  ##
##################################################################
caudal_diario     <- Data_Qf_m3s %>%
  mutate(mon_wy=wym_simple(date)) %>%
  data.table()

##################################################################
##                  CAUDAL MEDIO MENSUAL (m3/s)                ##
##################################################################
 #  MELADO
caudal_mensual_melado <-readRDS(filename_Q_mensual_melado) %>%
  mutate(mon_wy=wym_simple(as.Date(paste0(yr(mes,wy),"-",mes,"-01"))),
         Melado=QMM)%>%
  select(-mes,-QMM) %>% 
  data.table(key=c("wy","mon_wy"))

# THE REST and join with MELADO
caudal_mensual <- caudal_diario %>%
  group_by(wy,mon_wy) %>%
  summarise(Ancoa        = mean(Ancoa,na.rm = T),
            Achibueno    = mean(Achibueno,na.rm = T),
            Longavi      = mean(Longavi,na.rm = T),
            Maule        = mean(Maule,na.rm = T),
            Lontue       = mean(Lontue,na.rm = T))%>%
  data.table(key=c("wy","mon_wy")) %>%
  caudal_mensual_melado[.]


# # SAVE PARTIAL RESULT OF THE STREAMFLOW ORGANISED BY MONTHLY COLUMNS, YEARLY ROWS
# lapply(c("Achibueno","Ancoa","Lontue","Maule","Melado","Longavi"),
#        function(cuenca){
#          filename_export=paste0("data_auxiliar/Caudal_medio_mensual_2001_2020_",cuenca,"_rellenado.csv")
#          
#          melt_caudal_mensual=caudal_mensual %>%
#            melt.data.table(id.vars=c("wy","mon_wy")) %>%
#            subset(variable==cuenca & wy>1000) %>%
#            dcast.data.table(wy~mon_wy)%>%
#            .[, cuenca:=cuenca]
#          
#          write.csv(melt_caudal_mensual,filename_export,row.names=FALSE)
#          return(melt_caudal_mensual)
#        }) %>% rbindlist %>%
#   feather::write_feather(paste0("data_output/caudal_medio_mensual_1988_presente.feather"))

##################################################################
##                 VOLUMEN MENSUAL (MILL M3)                  ##
##################################################################
# MELADO
vol_mensual_melado<- readRDS(filename_Q_mensual_melado) %>%
  mutate(Melado=QMM*dayofmonth(mes,wy)*86400/1000000,
         mon_wy=wym_simple(as.Date(paste0(yr(mes,wy),"-",mes,"-01"))))%>%
  select(-QMM,-mes) %>% 
  data.table(key=c("wy","mon_wy"))

# THE REST and join with MELADO
vol_mensual       <- caudal_diario %>% group_by(wy,mon_wy) %>%
  summarise(Ancoa        = sum(Ancoa)*86400/1000000,
            Achibueno    = sum(Achibueno)*86400/1000000,
            Longavi      = sum(Longavi)*86400/1000000,
            Maule        = sum(Maule)*86400/1000000,
            Lontue       = sum(Lontue)*86400/1000000)%>%
  data.table(key=c("wy","mon_wy")) %>%
  vol_mensual_melado[.]

##################################################################
##                 VOLUMEN ESTACIONAL SEP-MAR &&   (MILL M3)                  ##
##################################################################

vol_mensual_exportar=c()
# COMPUTE CUMMULATIVE VOLUME FROM SEPTEMBER(=6) TO SEP, OCT, NOV, DEC, JAN, FEB, MAR 
for (imes in seq(6,12)) {
  vol_mensual_exportar    <- vol_mensual %>%
  subset(mon_wy %in% seq(6,imes))%>%
  group_by(wy)%>%
  summarise( Ancoa        = sum(Ancoa,na.rm = F),
             Achibueno    = sum(Achibueno,na.rm = F),
             Longavi      = sum(Longavi,na.rm = F),
             Melado       = sum(Melado,na.rm = F),
             Maule        = sum(Maule,na.rm = F),
             Lontue       = sum(Lontue,na.rm = F)) %>%
  transform(GRP=paste0("[",6,"-",imes,"]")) %>%
    rbind(vol_mensual_exportar)
}
# COMPUTE CUMMULATIVE VOLUME FROM OCT, NOV, DEC, JAN, FEB, MAR TO MARCH

for (imes in seq(7,12)) {
  vol_mensual_exportar    <- vol_mensual %>%
    subset(mon_wy %in% seq(imes,12))%>%
    group_by(wy)%>%
    summarise( Ancoa        = sum(Ancoa,na.rm = F),
               Achibueno    = sum(Achibueno,na.rm = F),
               Longavi      = sum(Longavi,na.rm = F),
               Melado       = sum(Melado,na.rm = F),
               Maule        = sum(Maule,na.rm = F),
               Lontue       = sum(Lontue,na.rm = F)) %>%
    mutate(GRP=paste0("[",imes,"-",12,"]")) %>%
    rbind(vol_mensual_exportar)
}


feather::write_feather(vol_mensual_exportar,paste0("data_input/vol_estacional_1978_2020.feather"))

# EXCEDENCE PROBABILITY OF VOLUME OCT-MAR DETERMINISTIC PREDICTION

vol_estacional2 <- vol_mensual_exportar %>% subset(GRP=="[7-12]")

1-ecdf(vol_estacional2$Ancoa)(300)
1-ecdf(vol_estacional2$Achibueno)(487)
1-ecdf(vol_estacional2$Longavi)(361)
1-ecdf(vol_estacional2$Melado)(2000)
1-ecdf(vol_estacional2$Maule)(3998)
1-ecdf(vol_estacional2$Lontue)(1092)

rm(Data_Qf_m3s,vol_mensual_melado)
