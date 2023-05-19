

library(ecmwfr)
library(lubridate)
library(dplyr)
library(magrittr)



####

## chequear fechas de termino de descarga porque el server ya no acepta fechas inexistentes (si no, error)
## ERA5 llega a -5 dias del presente (por desfase horario llega a -6 dias, si hay retrasos podria llegar a -7)

t_near <- Sys.time() %>% format(tz = "GMT", usetz = T) %>% as.POSIXct(tz = "GMT") %>%
  `-`(5 * 24 * 3600) %>% trunc(units = "hour") %>% as.POSIXct(tz = "GMT")

t_i    <- as.Date("2023-02-01")  # primer dia provisorio, el consolidado (ERA5 final, validado) llega a 2023-01-31 inclusive
t_f    <- as.Date(trunc(t_near - 13 * 3600 - 24 * 3600, units = "day"))  # ultimo dia provisorio completo

df_t   <- data.frame(date = seq(t_i, t_f, by = "day")) %>% mutate(mes = format(date, "%m"), agno = format(date, "%Y")) %>%
  group_by(mes) %>% mutate(days_month = days_in_month(date), n = n())

Meses_Agnos <- df_t %>% group_by(mes) %>% filter(days_month == n) %>%
  summarise(mes = unique(mes), agno = unique(agno))  # meses completos
Meses       <- Meses_Agnos$mes  # meses completos
Mes_Inc     <- df_t %>% filter(days_month != n) %$% mes %>% unique  # mes incompleto
Agno_Inc    <- df_t %>% filter(days_month != n) %$% agno %>% unique  # agno incompleto

save(t_i, t_f, Meses, file = "data_input/preproceso_meteo/output_preproceso_meteo/ti-tf.RData")

## ¿parche por actualizacion?
## ahora hay que fijar una keyring... revisar para cada computador...
## *** keyring: ukringu ***

# options(keyring_backend = "file")  # no funciona ¿? para remover keyring...

## lee user y key de servidor CDS (Copernicus Data Store) desde archivo txt

CDS_key <- as.data.frame(t(read.table("data_input/preproceso_meteo/input_preproceso_meteo/CDS_user_key", sep = "=", row.names = 1)))

## ingresa credenciales

wf_set_key(user = CDS_key$user, key = CDS_key$key, service = "cds")

## limpia carpeta de descargas (si no, error en codigos posteriores)

system("rm data_input/preproceso_meteo/download_meteo/*")

####

## descarga precipitacion y temperatura, por mes
## descarga distinta para meses completos y meses incompletos, y dias incompletos no se descargan
## modificar si la restriccion del servidor cambia en el futuro!

for (Var in c("total_precipitation", "2m_temperature")){
  for (Agno in unique(Meses_Agnos$agno)){
    for (Mes in filter(Meses_Agnos, agno %in% Agno)$mes){
      request <- list(
        product_type = "reanalysis",
        format = "netcdf",
        variable = Var,
        year = Agno,
        month = Mes,
        day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
                "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
        time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00",
                 "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00",
                 "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
        area = c(-27, -72, -37, -69),
        dataset_short_name = "reanalysis-era5-single-levels",
        target = paste0("data_input/preproceso_meteo/download_meteo/era5_", Var, "_", Agno, "-", Mes, ".nc")
      )
      file <- wf_request(user = CDS_key$user, request = request, transfer = TRUE, path = ".") 
    }
  }
}

for (Var in c("total_precipitation", "2m_temperature")){
  request <- list(
    product_type = "reanalysis",
    format = "netcdf",
    variable = Var,
    year = Agno_Inc,
    month = Mes_Inc,
    day = seq(1, as.numeric(format(t_near - 24 * 3600, "%d"))) %>% sprintf("%02d", .),
    time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00",
             "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00",
             "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
    area = c(-27, -72, -37, -69),
    dataset_short_name = "reanalysis-era5-single-levels",
    target = paste0("data_input/preproceso_meteo/download_meteo/era5_", Var, "_", Agno_Inc, "-", Mes_Inc, ".nc")
  )
  file <- wf_request(user = CDS_key$user, request = request, transfer = TRUE, path = ".")
}

####

## comandos en CDO (instalar en Linux/UNIX)

## elimina dimension provisoria y concatena archivos
system("cd data_input/preproceso_meteo/download_meteo; for f in era5_total_precipitation_*; do cdo -b F64 vertsum $f ERA5_total_precipitation_$(printf $f | cut --complement -c -25); done")
system("cd data_input/preproceso_meteo/download_meteo; cdo -b F64 mergetime ERA5_total_precipitation_* 'ERA5_precip_2023-presente.nc'")
system("cd data_input/preproceso_meteo/download_meteo; for f in era5_2m_temperature_*; do cdo -b F64 vertsum $f ERA5_2m_temperature_$(printf $f | cut --complement -c -20); done")
system("cd data_input/preproceso_meteo/download_meteo; cdo -b F64 mergetime ERA5_2m_temperature_* 'ERA5_temp_2023-presente.nc'")

## cambio de hora y nivel diario, ademas cambia unidades
system("cd data_input/preproceso_meteo/download_meteo; cdo -b F64 setattribute,tp@units=mm -mulc,1000 -daysum -shifttime,-13hour 'ERA5_precip_2023-presente.nc' 'ERA5_precip_2023-presente_daily.nc'")
system("cd data_input/preproceso_meteo/download_meteo; cdo -b F64 setattribute,t2m@units=Celsius -addc,-273.15 -daymean -shifttime,-12hour 'ERA5_temp_2023-presente.nc' 'ERA5_temp_2023-presente_daily.nc'")

## split por mes
system("cd data_input/preproceso_meteo/download_meteo; cdo splitmon 'ERA5_precip_2023-presente_daily.nc' 'ERA5_precip_2023-presente_daily_m'")
system("cd data_input/preproceso_meteo/download_meteo; cdo splitmon 'ERA5_temp_2023-presente_daily.nc' 'ERA5_temp_2023-presente_daily_m'")

## remueve
system("cd data_input/preproceso_meteo/download_meteo; rm ERA5_total_precipitation_*")
system("cd data_input/preproceso_meteo/download_meteo; rm ERA5_precip_2023-presente.nc")
system("cd data_input/preproceso_meteo/download_meteo; rm ERA5_precip_2023-presente_daily.nc")
system("cd data_input/preproceso_meteo/download_meteo; rm ERA5_2m_temperature_*")
system("cd data_input/preproceso_meteo/download_meteo; rm ERA5_temp_2023-presente.nc")
system("cd data_input/preproceso_meteo/download_meteo; rm ERA5_temp_2023-presente_daily.nc")


