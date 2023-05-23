rm(list = ls())
library(dplyr)

# GENERAL
atributos = read.csv("data_input/attributes/attributes_45catchments_ChileCentral.csv")

# DGA
scores_cuencas_DGA = read.csv(file = "data_output/figuras/pronostico_DGA/scores_DGA.csv")
vol_DGA = read.csv(file = "data_output/figuras/pronostico_DGA/volumen_obs_pronosticado_DGA.csv")
wys = vol_DGA$wy %>% unique()
catchment_codes = unique(vol_DGA$cod_cuenca) %>% sort()

cuencas_comunes = 
atributos %>%
  select(cod_cuenca,gauge_name) %>% 
  filter(cod_cuenca %in% catchment_codes)
  

#UCHILE
vol_forecast = read.csv(file = "data_output/figuras/pronostico_DGA/volumen_obs_pronosticado_uchile.csv")%>%
  filter(wy_simple %in% wys)

p1 = vol_DGA %>%
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
    subtitle = "Periodo 1990/91-2019/20. 9 cuencas incluidas"
  )

p1_ = p1 +
  geom_label(mapping = aes(x = 9000,y =9000,label = "y=x"),col = "black")+
  coord_equal(xlim = c(0,10000),ylim = c(0,10000))

p1__ = p1 + facet_wrap(~cod_cuenca,scales = "free")
plot(p1__)

ggsave(filename = "data_output/figuras/pronostico_DGA/scatter_obs_pronosticado_DGA.png",
       width = 7,height = 5, plot = p1_)

ggsave(filename = "data_output/figuras/pronostico_DGA/scatter_obs_pronosticado_DGA_cuencas.png",
       width = 7,height = 5, plot = p1__)

p2 = vol_forecast %>%
  dplyr::rename(PRO = volume_prom_pronostico) %>% 
  dplyr::rename(OBS = volume_original) %>%
  dplyr::rename(wy = wy_simple) %>% 
  subset(wy<2020) %>%
  ggplot(aes(y = OBS,x = PRO, col = wy))+
  geom_point()+
  scale_color_viridis_b()+
  geom_abline(slope = 1,intercept = 0)+
  labs(
    y = "volumen observado (mill. m3)",
    x = "volumen pronosticado uchile (mill. m3)",
    col = "Década emisión",
    title = "Volúmenes sep-mar pronosticados uchile vs obs",
    subtitle = "Periodo 1990/91-2019/20. 9 cuencas incluidas"
  )

p2_=p2+
  geom_label(mapping = aes(x = 9000,y =9000,label = "y=x"),col = "black")+
  coord_equal(xlim = c(0,10000),ylim = c(0,10000))

p2__ = p2 + facet_wrap(~catchment_code,scales = "free")
plot(p2__)

ggsave(filename = "data_output/figuras/pronostico_DGA/scatter_obs_pronosticado_uchile.png",
       width = 7,height = 5,plot = p2_)

ggsave(filename = "data_output/figuras/pronostico_DGA/scatter_obs_pronosticado_uchile_cuencas.png",
       width = 7,height = 5,plot = p2__)
######

# calcular metricas
source("base/Scores.R")

#metricas universales
metricas_univariables_DGA = 
  deterministic_scores(
  y_true = vol_DGA$volume_obs_GL,
  y_pred = vol_DGA$volume_GL) %>% 
  data.frame()

metricas_univariables_uchile = deterministic_scores(
  y_true = vol_forecast$volume_original,
  y_pred = vol_forecast$volume_prom_pronostico) %>% 
  data.frame()

#metricas por cuencas
metricas_cuenca_DGA <- function(catchment_code) {
  # datos de una cuenca  
  volumen_DGA = vol_DGA %>% 
    subset(cod_cuenca == catchment_code)
  # metricas para una cuenca, DGA
  metricas_univariables = deterministic_scores(
    y_true = volumen_DGA$volume_obs_GL,
    y_pred = volumen_DGA$volume_GL) %>% 
    data.frame() %>% 
    mutate(cod_cuenca = catchment_code) %>% 
    mutate(origen = "DGA")
  
  return(metricas_univariables)
}

metricas_cuenca_uchile <- function(code) {
  # datos de una cuenca  
  volumen_uchile = vol_forecast %>% 
    subset(catchment_code %in% code)
  # metricas para una cuenca, DGA
  metricas_univariables = deterministic_scores(
    y_true = volumen_uchile$volume_original,
    y_pred = volumen_uchile$volume_prom_pronostico) %>% 
    data.frame() %>% 
    mutate(cod_cuenca = code) %>% 
    mutate(origen = "uchile")
  
  return(metricas_univariables)
}

metricas_univariables = 
rbind(
lapply(catchment_codes, metricas_cuenca_DGA) %>% rbindlist(),
lapply(catchment_codes, metricas_cuenca_uchile) %>% rbindlist()
) %>%
  data.table() %>% 
  melt.data.table(id.vars = c("cod_cuenca","origen"))

metricas_univariables$variable = factor(
  metricas_univariables$variable,
  labels = c("RMSE (GL)","R2","MAE (GL)","pBIAS (-)"),
  levels = c("rmse_avg","r2_avg", "mae_avg", "pbias_avg"))

p3 = ggplot(data = metricas_univariables,aes(x = origen,y = value))+
  geom_boxplot()+
  facet_wrap(~variable,scales = "free_y")+
  labs(
    x = "",
    y = ""
  )
  

ggsave(filename = "data_output/figuras/pronostico_DGA/metricas_comparacion_univariables.png",
       width = 7,height = 5,plot = p3)

