####
rm(list = ls())
library(dplyr)
library(ncdf4)
library(sf) # version 1.0-9
library(terra) # version 1.6-47
library(exactextractr) # version 0.9.0
library(magrittr)


## 45 cuencas chile central

cuencas <- st_read("data_input/preproceso_meteo/input_preproceso_meteo/cuencas_fondef.shp")
Attr <- read.table("data_input/preproceso_meteo/input_preproceso_meteo/Atributos_Cuencas_Fondef.csv", sep = ",", header = T)

## periodo

load("data_input/preproceso_meteo/output_preproceso_meteo/ti-tf.RData")

## crea series promedio cuenca

era5pr_val <- vector("list", length(Meses)) %>% setNames(Meses)
era5tm_val <- vector("list", length(Meses)) %>% setNames(Meses)


for (Mes in Meses) {
  ## ERA5 precip
  namef <- paste0("data_input/preproceso_meteo/download_meteo/ERA5_precip_2023-presente_daily_m", Mes, ".nc")
  
  nc <- nc_open(namef)
  
  t_d <- as.Date(as.POSIXct("1900-01-01 00:00:00", tz = "UTC") + 3600 * ncvar_get(nc, "time"))
 a = ncvar_get(nc, "tp")
 
 
 
  era5pr_val[[Mes]] <- exact_extract(rast(namef), cuencas, "mean") %>%
    t() %>%
    structure(dimnames = list(NULL, cuencas$gauge_id)) %>%
    cbind.data.frame(date = t_d, .) %>%
    filter(date >= t_i & date <= t_f) %>% 
    mutate(across(-date, ~ round(., 8)))
  
  nc_close(nc)
  
  ## ERA5 temp
  namef <- paste0("data_input/preproceso_meteo/download_meteo/ERA5_temp_2023-presente_daily_m", Mes, ".nc")
  
  nc <- nc_open(namef)
  
  t_d <- as.Date(as.POSIXct("1900-01-01 00:00:00", tz = "UTC") + 3600 * ncvar_get(nc, "time"))
  
  era5tm_val[[Mes]] <- exact_extract(rast(namef), cuencas, "mean") %>%
    t() %>%
    structure(dimnames = list(NULL, cuencas$gauge_id)) %>%
    cbind.data.frame(date = t_d, .) %>%
    filter(date >= t_i & date <= t_f) %>% 
    mutate(across(-date, ~ round(., 8)))
  
  nc_close(nc)
}

rm(Mes, namef, nc, t_d)

save(era5pr_val, era5tm_val, file = "data_input/preproceso_meteo/output_preproceso_meteo/series-escala-cuenca_fondef_2023-presente.RData")
