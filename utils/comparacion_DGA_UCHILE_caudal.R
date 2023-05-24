rm(list = ls())
library(dplyr)

# GENERAL
#atributos = read.csv("data_input/attributes/attributes_45catchments_ChileCentral.csv")
months_year <- c("ene", "feb", "mar","abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic")

# DGA
cuencas_comunes = read.csv("data_input/flows/cuencas_pronosticos_DGA_largoregistro.csv") %>%
  select(gauge_name,cod_cuenca,area_km2)
  
caudal_pronosticado_DGA = read.csv("data_output/figuras/pronostico_DGA/caudal_pronosticado_DGA.csv") %>% 
  select(-c(year,ym,wym,gauge_name,area_km2)) %>% 
  dplyr::rename(q_dga = Q_m3s)

wys_dga = unique(caudal_pronosticado_DGA$wy)

#UCHILE

source("utils/main_regression_model.R")
caudal_pronosticado_uchile = lapply(cuencas_comunes$cod_cuenca,run_model) %>% 
  rbindlist %>%
  subset(wy %in% wys_dga) %>% 
  dplyr::rename(month_name = month) %>% 
  mutate(month = match(month_name, months_year)) %>% 
  select(month,catchment_code, everything(),-month_name) %>% 
  mutate(wy = as.numeric(wy))
  
caudal_dga_uchile = merge.data.table(caudal_pronosticado_DGA,
                                     caudal_pronosticado_uchile,
                                     by.y = c("month","catchment_code","wy"),
                                     by.x = c("month","cod_cuenca","wy"))


#### caudal promedio del ensemble
caudal_dga_uchile_promedio = 
  caudal_dga_uchile %>%
  mutate(ens_avg = rowMeans(select_if(., is.numeric) %>% select(starts_with('X'))
  )) %>% 
  select(-starts_with('X'))

plot_scatters <- function() {
  

### scatter simulado vs obs
p1 = caudal_dga_uchile_promedio %>%
  ggplot(aes(y = q_obs,x = q_dga, col = wy))+
  geom_point()+
  scale_color_viridis_b()+
  geom_abline(slope = 1,intercept = 0)+
  labs(
    y = "caudal observado (m3/s)",
    x = "caudal pronosticado DGA (m3/s)",
    col = "Década emisión",
    title = "Caudales medios mensuales sep-mar pronosticados DGA vs obs",
    subtitle = "Periodo 1990/91-2019/20. 9 cuencas incluidas"
  )

p1_ = p1 +
  geom_label(mapping = aes(x = 700,y =700,label = "y=x"),col = "black")+
  coord_equal()

p1__ = p1 + facet_wrap(~cod_cuenca,scales = "free")
#plot(p1_)

ggsave("data_output/figuras/pronostico_DGA/scatter_caudales_obs_pronosticado_DGA.png",
       width = 7, height = 5, plot = p1_)

ggsave("data_output/figuras/pronostico_DGA/scatter_caudales_obs_pronosticado_DGA_cuencas.png",
       width = 7, height = 5, plot = p1__)

#### uchile
### scatter simulado vs obs
p2 = caudal_dga_uchile_promedio %>%
  ggplot(aes(y = q_obs,x = ens_avg, col = wy))+
  geom_point()+
  scale_color_viridis_b()+
  geom_abline(slope = 1,intercept = 0)+
  labs(
    y = "caudal observado (m3/s)",
    x = "caudal pronosticado uchile (m3/s)",
    col = "Década emisión",
    title = "Caudales medios mensuales sep-mar pronosticados uchile vs obs",
    subtitle = "Periodo 1990/91-2019/20. 9 cuencas incluidas"
  )

p2_ = p2 +
  geom_label(mapping = aes(x = 700,y =700,label = "y=x"),col = "black")+
  coord_equal()

p2__ = p2 + facet_wrap(~cod_cuenca,scales = "free")
#plot(p1_)

ggsave("data_output/figuras/pronostico_DGA/scatter_caudales_obs_pronosticado_uchile.png",
       width = 7, height = 5, plot = p2_)

ggsave("data_output/figuras/pronostico_DGA/scatter_caudales_obs_pronosticado_uchile_cuencas.png",
       width = 7, height = 5, plot = p2__)
}

plot_scatters()



scores_deterministicos_por_cuenca <- function(code) {
  
  caudal_cuenca = caudal_dga_uchile_promedio %>%
    filter(cod_cuenca == code)

  metricas_uni_dga = deterministic_scores(
    y_true = caudal_cuenca$q_obs,
    y_pred = caudal_cuenca$q_dga) %>% 
    data.frame() %>% 
    mutate(cod_cuenca = code) %>% 
    mutate(origen = "DGA")
  
  metricas_uni_uchile = deterministic_scores(
    y_true = caudal_cuenca$q_obs,
    y_pred = caudal_cuenca$ens_avg) %>% 
    data.frame() %>% 
    mutate(cod_cuenca = code) %>% 
    mutate(origen = "uchile")
  
  metricas_univariables = rbind(metricas_uni_dga,
                                metricas_uni_uchile)
  
  return(metricas_univariables)

}

scores_deterministicos_por_mes <- function(mes) {
  
  caudal_cuenca = caudal_dga_uchile_promedio %>%
    filter(month == mes)
  
  metricas_uni_dga = deterministic_scores(
    y_true = caudal_cuenca$q_obs,
    y_pred = caudal_cuenca$q_dga) %>% 
    data.frame() %>% 
    mutate(month = mes) %>% 
    mutate(origen = "DGA")
  
  metricas_uni_uchile = deterministic_scores(
    y_true = caudal_cuenca$q_obs,
    y_pred = caudal_cuenca$ens_avg) %>% 
    data.frame() %>% 
    mutate(month = mes) %>% 
    mutate(origen = "uchile")
  
  metricas_univariables = rbind(metricas_uni_dga,
                                metricas_uni_uchile)
  
  return(metricas_univariables)
}


metricas_univariables = 
  lapply(cuencas_comunes$cod_cuenca,scores_deterministicos_por_cuenca ) %>% 
  rbindlist()

metricas_univariables_long = metricas_univariables %>% 
  melt.data.table(id.vars = c("cod_cuenca","origen"))

metricas_univariables_long$variable = factor(
  metricas_univariables_long$variable,
  labels = c("RMSE (m3/s)","R2","MAE (m3/s)","pBIAS (-)"),
  levels = c("rmse_avg","r2_avg", "mae_avg", "pbias_avg"))

p3 = ggplot(data = metricas_univariables_long,aes(x = origen,y = value))+
  geom_boxplot()+
  facet_wrap(~variable,scales = "free_y")+
  labs(
    x = "",
    y = ""
  )
ggsave(filename = "data_output/figuras/pronostico_DGA/metricas_comparacion_univariables_caudal.png",
       width = 7,height = 5,plot = p3)
##########
##### CRPSS

scores_ensemble_por_cuenca <- function(code) {
  print(code)
  # filtrar por cuenca
  caudal_cuenca = caudal_dga_uchile %>%
      filter(cod_cuenca == code)

  # datos de una cuenca
  y_train = caudal_cuenca$q_obs %>% as.matrix()

  y_ens =  caudal_cuenca %>%
    dplyr::select(starts_with("X"))
  # calcular metricas
  metricas_ensemble= ensemble_scores(y_train = y_train, y_ens = y_ens) %>%
    data.frame() %>%
    mutate(cod_cuenca = code)

  return(metricas_ensemble)
}

# metricas probabilisticas
scores_ens_uchile = lapply(sort(cuencas_comunes$cod_cuenca), scores_ensemble_por_cuenca) %>%
  rbindlist()

#####CRPSS
#CRPS
crps_uchile = scores_ens_uchile %>%
  select(cod_cuenca,crps_ens)
#MAE
mae_DGA = metricas_univariables %>%
  subset(origen == "DGA") %>%
  select(cod_cuenca, mae_avg)
#CRPSS
crps_DGA_uchile = merge(crps_uchile, mae_DGA) %>%
  mutate(crpss = 1 - crps_ens / mae_avg) %>%
  merge(cuencas_comunes) %>%
  mutate(gauge_name = factor(gauge_name, levels = crps_DGA_uchile$gauge_name[order(crps_DGA_uchile$cod_cuenca)]))

ggplot(crps_DGA_uchile, aes(y = gauge_name, x = crpss))+
  geom_point()+
  geom_vline(xintercept = 0)+
  labs( x = "CRPSS c/r pronóstico DGA",
        y = "")

ggsave(filename = "data_output/figuras/pronostico_DGA/crpss_comparacion_caudal.png",
       width = 7, height = 5)



###CRPSS por mes

scores_ensemble_por_mes <- function(mes) {
  print(mes)
  # filtrar por cuenca
  caudal_cuenca = caudal_dga_uchile %>%
    filter(month == mes)
  
  # datos de un mes
  y_train = caudal_cuenca$q_obs %>% as.matrix()
  
  y_ens =  caudal_cuenca %>%
    dplyr::select(starts_with("X"))
  # calcular metricas
  metricas_ensemble= ensemble_scores(y_train = y_train, y_ens = y_ens) %>%
    data.frame() %>%
    mutate(month = mes)
  
  return(metricas_ensemble)
}

# metricas probabilisticas
meses_pronosticos = c(9,10,11,12,1,2,3)
scores_ens_uchile_mes = lapply(meses_pronosticos, scores_ensemble_por_mes) %>%
  rbindlist()
metricas_univariables_mes = lapply(meses_pronosticos, scores_deterministicos_por_mes) %>%
  rbindlist()

#####CRPSS
#CRPS
crps_uchile = scores_ens_uchile_mes %>%
  select(month,crps_ens)



#MAE
mae_DGA = metricas_univariables_mes %>%
  subset(origen == "DGA") %>%
  select(month, mae_avg)
#CRPSS
crps_DGA_uchile = merge(crps_uchile, mae_DGA) %>%
  mutate(crpss = 1 - crps_ens / mae_avg) %>%
  mutate(month = factor(months_year[month],
                        levels = (months_year[meses_pronosticos])))
  
  

ggplot(crps_DGA_uchile, aes(x = month, y = crpss))+
  geom_point()+
  geom_vline(xintercept = 0)+
  labs( y = "CRPSS c/r pronóstico DGA",
        x = "mes pronosticado")+
  ylim(0,0.3)

ggsave(filename = "data_output/figuras/pronostico_DGA/crpss_mes_comparacion_caudal.png",
       width = 7, height = 5)

