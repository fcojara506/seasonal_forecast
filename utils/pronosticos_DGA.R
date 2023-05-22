rm(list = ls())
library(dplyr)
library(data.table)
library(ggplot2)
library(stringr)
library(lubridate)

source("base/Convert_units.R")
source("base/DatesWaterYear.R")

cuencas_pronostico_DGA = read.csv(file = "data_input/flows/cuencas_pronosticos_DGA.csv")
cuencas_pronostico = read.csv(file = "data_input/attributes/attributes_45catchments_ChileCentral.csv")

cuencas_pronosticos_merged = merge(cuencas_pronostico,cuencas_pronostico_DGA,by.x = "cod_cuenca",by.y = "gauge_id") %>% 
  select(cod_cuenca,gauge_name.y,area_km2) %>% 
  dplyr::rename(gauge_name = gauge_name.y) %>% 
  mutate(gauge_name =  stringr::str_trim(gauge_name))
rm(cuencas_pronostico_DGA,cuencas_pronostico)
# leer pronosticos
pronosticos_caudales_DGA = read.csv(file = "data_input/flows/pronosticos_DGA_m3s.csv",check.names = F) %>% 
  select(-Mes) %>% 
  dplyr::rename(wy = año) %>% 
  dplyr::rename(month = mes) %>%
  mutate(wym = wateryearmonth(month)) %>% 
  mutate(year = wateryear2year(wy = wy, wym = wateryearmonth(month))) %>% 
  mutate(ym = lubridate::make_date(year,month)) %>% 
  data.table() %>% 
  melt.data.table(id.vars = c("year","month","ym","wy","wym"),
                  variable.name = "gauge_name",
                  value.name = "Q_m3s") %>% 
  na.omit()

# pronosticos disponibles
datos_pronosticos_DGA = pronosticos_caudales_DGA %>% 
  mutate(hay_data = ifelse(Q_m3s>0 & !is.na(Q_m3s),"Data","No Data"))

ggplot(data = datos_pronosticos_DGA,aes(x = ym, y = gauge_name,fill=hay_data))+
  geom_tile()+
  labs(
    x = "fecha pronosticada",
    y = "",
    fill = "",
    title = "Fechas con caudales medios mensuales pronosticados"
  )

ggsave(filename = "data_output/figuras/pronostico_DGA/pronosticos_disponibles_DGA.png",
       width = 7,height = 5)

# filtrar cuencas con mayor cantidad de datos
cuencas_mas_datos = datos_pronosticos_DGA %>% 
  group_by(gauge_name) %>% 
  summarise(cuenta  = sum(hay_data == "Data")) %>% 
  filter(cuenta>200)%>% 
  merge(cuencas_pronosticos_merged)

# caudales medios mensuales de cuencas con mas datos
pronosticos_caudales_DGA_masdatos = pronosticos_caudales_DGA %>% 
  filter(gauge_name %in% cuencas_mas_datos$gauge_name) %>% 
  merge(cuencas_pronosticos_merged)

# calcular volumen sep-mar

volumeGL_decaudal <- function(catchment_code) {

Q_DGA = pronosticos_caudales_DGA_masdatos %>% 
  filter(cod_cuenca == catchment_code) %>% 
  na.omit()

q_dga =  
dcast.data.table(Q_DGA,wy ~ wym,value.var = "Q_m3s")

days_per_month = Q_DGA$month %>% unique() %>% lubridate::days_in_month()

Q_mm = convert_flow(q = select(q_dga,-wy), 
                    from = "m3/s",
                    to = "mm/month",
                    area_km2 = unique(Q_DGA$area_km2),
                    days_per_month = days_per_month,
                    ) %>% 
  mutate(wy = q_dga$wy) %>%
  data.table() %>% 
  melt.data.table(id.vars = "wy",variable.name = "wym",value.name = "Q_mm")

#compute volume
y <- stats::aggregate(
  x = Q_mm ~ wy ,
  FUN = "sum",
  data = Q_mm
) %>% dplyr::rename(volume_GL = Q_mm)

y_converted <- 
  convert_vol(
    v = select(y,-wy),
    from = "mm",
    to = "GL",
    area_km2 = unique(Q_DGA$area_km2)
  )
y_converted$wy = y$wy
y_converted$cod_cuenca = unique(Q_DGA$cod_cuenca)
y_converted = select(y_converted,cod_cuenca,wy,volume_GL)
return(y_converted)
}
# aplicar para todas las cuencas
catchment_codes = unique(cuencas_mas_datos$cod_cuenca,1) %>% sort()
pronosticos_volumenes_DGA = lapply(catchment_codes,volumeGL_decaudal) %>% 
  rbindlist()
 
# leer volumen observado
volumen_obs = read.csv(file = "data_input/flows/volume_sepmar_mm_GL_45catchments_ChileCentral.csv") %>%
  select(-volume_mm) %>% 
  dplyr::rename(volume_obs_GL = volume_GL)

volumen_obs_pronosticado = merge.data.table(pronosticos_volumenes_DGA,volumen_obs,by.x = c("wy","cod_cuenca"),by.y = c("wy_simple","cod_cuenca") )


volumen_obs_pronosticado %>%
  dplyr::rename(DGA = volume_GL) %>% 
  dplyr::rename(OBS = volume_obs_GL) %>%
  subset(wy<2020) %>%
  ggplot(aes(y = OBS,x = DGA, col = wy))+
  geom_point()+
  scale_color_viridis_b()+
  geom_abline(slope = 1,intercept = 0)+
  labs(
    y = "volumen observado (mill. m3)",
    x = "volumen pronosticado DGA (mill. m3)",
    col = "Década emisión",
    title = "Volúmenes sep-mar pronosticados DGA vs obs",
    subtitle = "Periodo 1990/91-2019/20. 10 cuencas incluidas"
  )+
  geom_label(mapping = aes(x = 10000,y =10000,label = "y=x"),col = "black")+
  coord_equal(xlim = c(0,13000),ylim = c(0,13000))


ggsave(filename = "data_output/figuras/pronostico_DGA/scatter_obs_pronosticado_DGA.png",
       width = 7,height = 5)
# calcular metricas
source("base/Scores.R")

#metricas universales
metricas_univariables = deterministic_scores(
  y_true = volumen_obs_pronosticado$volume_obs_GL,
  y_pred = volumen_obs_pronosticado$volume_GL) %>% 
  data.frame()
#metricas por cuencas

metricas_cuenca <- function(catchment_code) {
 # datos de una cuenca  
  volumen_obs_pronosticado = volumen_obs_pronosticado %>% 
    subset(cod_cuenca == catchment_code)
  # metricas para una cuenca
  metricas_univariables = deterministic_scores(
    y_true = volumen_obs_pronosticado$volume_obs_GL,
    y_pred = volumen_obs_pronosticado$volume_GL) %>% 
    data.frame() %>% 
    mutate(cod_cuenca = catchment_code)
  
  return(metricas_univariables)
}

scores_cuencas_DGA = lapply(catchment_codes, metricas_cuenca) %>% 
  rbindlist()

write.csv(scores_cuencas_DGA,file = "data_output/figuras/pronostico_DGA/scores_DGA.csv",row.names = F)
