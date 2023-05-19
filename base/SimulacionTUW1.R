####

library(dplyr)
library(TUWmodel)
library(airGR)

####

## Lee meteo al presente y obtiene evaporacion potencial (EP) con formula de Oudin

lats    <- read.table("data_input/preproceso_meteo/input_preproceso_meteo/latitudes-promedio_fondef.csv", sep = ",", header = T)
Temp_p  <- read.table("data_input/preproceso_meteo/output_preproceso_meteo/temp_ERA5-BC-ensemble-prom_N30_2023-presente.csv", sep = ",", header = T, check.names = F)

Cuencas <- names(Temp_p)[-1]
Dates_p <- as.Date(Temp_p[, "date"])

PE_p <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
for (Cuenca in Cuencas){
  Temp_Cuenca    <- pull(Temp_p, Cuenca)
  Lat_Cuenca     <- filter(lats, gauge_id %in% Cuenca)$Y_mean
  PE_p[[Cuenca]] <- PE_Oudin(as.numeric(format(Dates_p, "%j")), Temp_Cuenca, Lat_Cuenca, LatUnit = "deg")
}
rm(Cuenca, Temp_Cuenca, Lat_Cuenca)
PE_p <- cbind(date = Dates_p, do.call(cbind.data.frame, PE_p))

write.table(PE_p, file = "data_input/preproceso_meteo/output_preproceso_meteo/PE-Oudin_2023-presente.csv", sep = ",", row.names = F)
rm(lats, Temp_p, PE_p, Dates_p)

## Consolida meteo historica y al presente

Temp <- rbind(read.table("data_input/preproceso_meteo/input_preproceso_meteo/temp_ERA5-BC-ensemble-prom_N30_1979-2023.csv", sep = ",", header = T, check.names = F),
              read.table("data_input/preproceso_meteo/output_preproceso_meteo/temp_ERA5-BC-ensemble-prom_N30_2023-presente.csv", sep = ",", header = T, check.names = F))

Prec <- rbind(read.table("data_input/preproceso_meteo/input_preproceso_meteo/precip_ERA5-BC-ensemble-prom_N30_1979-2023.csv", sep = ",", header = T, check.names = F),
              read.table("data_input/preproceso_meteo/output_preproceso_meteo/precip_ERA5-BC-ensemble-prom_N30_2023-presente.csv", sep = ",", header = T, check.names = F))

PE   <- rbind(read.table("data_input/preproceso_meteo/input_preproceso_meteo/PE-Oudin_1979-2023.csv", sep = ",", header = T, check.names = F),
              read.table("data_input/preproceso_meteo/output_preproceso_meteo/PE-Oudin_2023-presente.csv", sep = ",", header = T, check.names = F))

## Parametros calibrados TUW

Pars <- read.table("data_input/preproceso_meteo/input_preproceso_meteo/parametrosTUW_calib1994-2019wys_FO-sKGE-slogNSE.csv", sep = ",", header = T, check.names = F)

## Simulacion TUW
## Periodo simulacion 1981-04-01/presente
## Ademas, warm-up 1979-04-01/1981-03-31 (2 wys)

t_i  <- as.Date("1981-04-01")
t_ii <- as.Date("1979-04-01")

Temp <- filter(Temp, date >= t_ii)
Prec <- filter(Prec, date >= t_ii)
PE   <- filter(PE, date >= t_ii)

Dates <- Temp$date

SSM <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
SWE <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
SUZ <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
SLZ <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
for (Cuenca in Cuencas){
  Sim <- TUWmodel(prec = pull(Prec, Cuenca), airt = pull(Temp, Cuenca), ep = pull(PE, Cuenca), param = pull(Pars, Cuenca))
  SSM[[Cuenca]] <- drop(Sim$moist)
  SWE[[Cuenca]] <- drop(Sim$swe)
  SUZ[[Cuenca]] <- drop(Sim$suz)
  SLZ[[Cuenca]] <- drop(Sim$slz)
}
rm(Cuenca, Sim)

SSM <- filter(cbind.data.frame(date = Dates, do.call(cbind, SSM)), date >= t_i)
SWE <- filter(cbind.data.frame(date = Dates, do.call(cbind, SWE)), date >= t_i)
SUZ <- filter(cbind.data.frame(date = Dates, do.call(cbind, SUZ)), date >= t_i)
SLZ <- filter(cbind.data.frame(date = Dates, do.call(cbind, SLZ)), date >= t_i)

write.table(SSM, file = "data_input/storage_variables/ERA5Ens/operacional/SSM.csv", sep = ",", row.names = F)
write.table(SWE, file = "data_input/storage_variables/ERA5Ens/operacional/SWE.csv", sep = ",", row.names = F)
write.table(SUZ, file = "data_input/storage_variables/ERA5Ens/operacional/SUZ.csv", sep = ",", row.names = F)
write.table(SLZ, file = "data_input/storage_variables/ERA5Ens/operacional/SLZ.csv", sep = ",", row.names = F)
