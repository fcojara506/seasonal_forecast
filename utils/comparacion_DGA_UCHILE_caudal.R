rm(list = ls())
library(dplyr)

# GENERAL
atributos = read.csv("data_input/attributes/attributes_45catchments_ChileCentral.csv")

# DGA
scores_cuencas_DGA = read.csv(file = "data_output/figuras/pronostico_DGA/scores_DGA.csv")
wys = NA
catchment_codes = unique(scores_cuencas_DGA$cod_cuenca) %>% sort()

cuencas_comunes = 
atributos %>%
  select(cod_cuenca,gauge_name) %>% 
  filter(cod_cuenca %in% catchment_codes)
  
#UCHILE

source("utils/main_regression_model.R")
a = lapply(cuencas_comunes$cod_cuenca,run_model) %>% 
  purrr::transpose()

flow_scores = rbindlist(a$scores_flow) %>%
  data.frame() %>% 
  mutate(cod_cuenca = cuencas_comunes$cod_cuenca)


