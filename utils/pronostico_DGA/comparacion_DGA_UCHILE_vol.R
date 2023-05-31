rm(list = ls())
library(dplyr)

# GENERAL
atributos = read.csv("data_input/attributes/attributes_45catchments_ChileCentral.csv")

# DGA
scores_cuencas_DGA = read.csv(file = "data_output/pronostico_DGA/scores_DGA.csv")
vol_DGA = read.csv(file = "data_output/pronostico_DGA/volumen_obs_pronosticado_DGA.csv")
wys = vol_DGA$wy %>% unique()
catchment_codes = unique(vol_DGA$cod_cuenca) %>% sort()

cuencas_comunes = 
atributos %>%
  select(cod_cuenca,gauge_name) %>% 
  filter(cod_cuenca %in% catchment_codes)
  

#UCHILE
vol_forecast = read.csv(file = "data_output/pronostico_DGA/volumen_obs_pronosticado_uchile.csv")%>%
  filter(wy_simple %in% wys)

p1 = vol_DGA %>%
  dplyr::rename(DGA = volume_GL) %>% 
  dplyr::rename(OBS = volume_obs_GL) %>%
  subset(wy<2020) %>%
  ggplot(aes(x = OBS,y = DGA, col = wy))+
  geom_point()+
  scale_color_viridis_b()+
  geom_abline(slope = 1,intercept = 0)+
  labs(
    x = "volumen observado (mill. m3)",
    y = "volumen pronosticado DGA (mill. m3)",
    col = "Década emisión",
    title = "Volúmenes sep-mar pronosticados DGA vs obs",
    subtitle = "Periodo 1990/91-2019/20. 9 cuencas incluidas"
  )

p1_ = p1 +
  geom_label(mapping = aes(x = 9000,y =9000,label = "y=x"),col = "black")+
  coord_equal(xlim = c(0,10000),ylim = c(0,10000))

p1__ = p1 + facet_wrap(~cod_cuenca,scales = "free")
plot(p1__)

ggsave(filename = "data_output/pronostico_DGA/scatter_obs_pronosticado_DGA.png",
       width = 7,height = 5, plot = p1_)

ggsave(filename = "data_output/pronostico_DGA/scatter_obs_pronosticado_DGA_cuencas.png",
       width = 7,height = 5, plot = p1__)

p2 = vol_forecast %>%
  dplyr::rename(PRO = volume_prom_pronostico) %>% 
  dplyr::rename(OBS = volume_original) %>%
  dplyr::rename(wy = wy_simple) %>% 
  subset(wy<2020) %>%
  ggplot(aes(x = OBS,y = PRO, col = wy))+
  geom_point()+
  scale_color_viridis_b()+
  geom_abline(slope = 1,intercept = 0)+
  labs(
    x = "volumen observado (mill. m3)",
    y = "volumen pronosticado uchile (mill. m3)",
    col = "Década emisión",
    title = "Volúmenes sep-mar pronosticados uchile vs obs",
    subtitle = "Periodo 1990/91-2019/20. 9 cuencas incluidas"
  )

p2_=p2+
  geom_label(mapping = aes(x = 9000,y =9000,label = "y=x"),col = "black")+
  coord_equal(xlim = c(0,10000),ylim = c(0,10000))

p2__ = p2 + facet_wrap(~catchment_code,scales = "free")
plot(p2__)

ggsave(filename = "data_output/pronostico_DGA/scatter_obs_pronosticado_uchile.png",
       width = 7,height = 5,plot = p2_)

ggsave(filename = "data_output/pronostico_DGA/scatter_obs_pronosticado_uchile_cuencas.png",
       width = 7,height = 5,plot = p2__)
######

# calcular metricas
source("base/Scores.R")

#metricas universales
# metricas_univariables_DGA = 
#   deterministic_scores(
#   y_true = vol_DGA$volume_obs_GL,
#   y_pred = vol_DGA$volume_GL) %>% 
#   data.frame()
# 
# metricas_univariables_uchile = deterministic_scores(
#   y_true = vol_forecast$volume_original,
#   y_pred = vol_forecast$volume_prom_pronostico) %>% 
#   data.frame()

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
  data.table()

metricas_univariables_long = metricas_univariables %>% 
  melt.data.table(id.vars = c("cod_cuenca","origen"))

metricas_univariables_long$variable = factor(
  metricas_univariables_long$variable,
  labels = c("RMSE (GL)","R2","MAE (GL)","pBIAS (-)"),
  levels = c("rmse_avg","r2_avg", "mae_avg", "pbias_avg"))

p3 = ggplot(data = metricas_univariables_long,aes(x = origen,y = value))+
  geom_boxplot()+
  facet_wrap(~variable,scales = "free_y")+
  labs(
    x = "",
    y = ""
  )
  

ggsave(filename = "data_output/pronostico_DGA/metricas_comparacion_univariables.png",
       width = 7,height = 5,plot = p3)

####### CRPSS


#source("utils/correr_modelo/main_regression_model.R")

# leer volumen observado
volumen_obs = read.csv(file = "data_input/flows/volume_mm_GL_45catchments_ChileCentral.csv") %>%
  select(-volume_mm) %>% 
  dplyr::rename(volume_obs_GL = volume_GL) 

# volumen pronosticado uchile
vol_ens = read.csv(file = "data_output/pronostico_DGA/volumen_obs_pronosticadoENS_uchile.csv",check.names = F) %>% 
  data.table() %>% 
  melt.data.table(id.vars = c("catchment_code"),
                  variable.name = "wy_simple",
                  value.name = "volume_GL") %>% 
  mutate(wy_simple = as.numeric(as.character(wy_simple)))
#unir
vol_ens_obs = merge.data.table(vol_ens,volumen_obs,
                 by.x = c("wy_simple","catchment_code"),
                 by.y = c("wy_simple","cod_cuenca") )

vol_ens_obs[, ens := row.names(.SD), by = c("wy_simple","catchment_code")]
vol_ens_obs =  vol_ens_obs %>% 
  dcast.data.table(wy_simple + catchment_code + volume_obs_GL ~ ens, value.var = "volume_GL")

rm(vol_ens)

# formato para ocupar en crpss
y_train = vol_ens_obs$volume_obs_GL %>% as.matrix()
y_ens = vol_ens_obs %>%
  select(-wy_simple,-catchment_code,-volume_obs_GL) %>% 
  as.matrix()

ensemble_scores(y_train = y_train, y_ens = y_ens)

metricas_cuenca <- function(code) {
  vol_ens_obs = 
    vol_ens_obs %>%
    subset(code == catchment_code)
  # datos de una cuenca  
  y_train = vol_ens_obs$volume_obs_GL %>% as.matrix()
  y_ens = vol_ens_obs %>%
    select(-wy_simple,-catchment_code,-volume_obs_GL) %>% 
    as.matrix()
  
 metricas_ensemble= ensemble_scores(y_train = y_train, y_ens = y_ens) %>% 
    data.frame() %>% 
    mutate(cod_cuenca = code)
  
  return(metricas_ensemble)
}

scores_ens_uchile = lapply(catchment_codes, metricas_cuenca) %>% 
  rbindlist() 

crps_uchile = scores_ens_uchile %>% 
  select(cod_cuenca,crps_ens)

mae_DGA = metricas_univariables %>% 
  subset(origen == "DGA") %>% 
  select(cod_cuenca, mae_avg)

library(forcats)

crps_DGA_uchile = merge(crps_uchile, mae_DGA) %>%
  mutate(crpss = 1 - crps_ens / mae_avg) %>%
  merge(cuencas_comunes)

library(forcats)

crps_DGA_uchile = merge(crps_uchile, mae_DGA) %>%
  mutate(crpss = 1 - crps_ens / mae_avg) %>%
  merge(cuencas_comunes) %>%
  mutate(gauge_name = factor(gauge_name, levels = crps_DGA_uchile$gauge_name[order(crps_DGA_uchile$cod_cuenca)]))

ggplot(crps_DGA_uchile, aes(y = gauge_name, x = crpss))+
  geom_point()+
  geom_vline(xintercept = 0)+
  labs( x = "CRPSS c/r pronóstico DGA",
        y = "")

ggsave(filename = "data_output/pronostico_DGA/crpss_comparacion.png",
       width = 7, height = 5)
