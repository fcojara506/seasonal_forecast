rm(list = ls())
library(dplyr)
library(data.table)
library(ggplot2)
library(stringr)
library(lubridate)

source("base/Convert_units.R")
source("base/DatesWaterYear.R")

attributes_catchment = read.csv(file = "data_input/attributes/attributes_45catchments_ChileCentral.csv") %>% 
  select(cod_cuenca,gauge_name,area_km2)

caudales_cuencas_mm =
  read.csv(file = "data_input/flows/flows_mm_monthly_49catchments_ChileCentral.csv") %>% 
  filter(wym>5)

#volumen 
y <- stats::aggregate(
  x = Q_mm ~ wy_simple+cod_cuenca ,
  FUN = "sum",
  data = caudales_cuencas_mm
) %>% dplyr::rename(volume_mm = Q_mm) %>% 
  merge(select(attributes_catchment,cod_cuenca,area_km2)) %>% 
  mutate(volume_GL = volume_mm*area_km2*0.001) %>% 
  select(-area_km2)

write.csv(y, file = "data_input/flows/volume_mm_GL_45catchments_ChileCentral.csv",row.names = F)
