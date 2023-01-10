rm(list = ls())

source("utils/run_model_function.R")
#test catchments
catchments_attributes_filename = "data_input/attributes/attributes_49catchments_ChileCentral.feather" 
attributes_catchments = feather::read_feather(catchments_attributes_filename)

cod_cuencas = attributes_catchments$cod_cuenca
#test months initial
months_initialisation = c('may','jun','jul','ago','sep','oct','nov','dic','ene','feb')

# test climate indices
# climate indices
climate_indices = c(
  "MEIv2",
  "PDO",
  "SOI",
  "ONI",
  "NINO1.2",
  "NINO3",
  "NINO4",
  "NINO3.4",
  "ESPI",
  "AAO",
  "BIENSO"
)
# generate predictor from standard climate index
climate_predictors = c(
  paste(climate_indices,"mean",paste0(1,"months"),sep = "_")
  #paste(climate_indices,"mean",paste0(-1,"months"),sep = "_")
)

library(foreach)
library(doParallel)
registerDoParallel(cores=8)

model <-
  foreach(month_initialisation=months_initialisation,.combine = "c") %:%
  foreach(climate_predictor = climate_predictors,.combine = "c") %:%
  foreach(catchment_code=cod_cuencas) %dopar% {
    
    run_model(
      catchment_code = catchment_code,
      month_initialisation = month_initialisation,
      horizon_month_start = "sep",
      horizon_month_end = "mar",
      horizon_strategy = "static",
      predictor_list = climate_predictor,
      wy_holdout = 2022,
      remove_wys = NA,
      units_q = "m3/s",
      units_y = "GL",
      test_subset = F
    )
    
  } %>% purrr::transpose()

stopImplicitCluster()

scores = rbindlist(model$scores)
info = rbindlist(model$info)

data_input = cbind(info,scores) %>%
  merge(attributes_catchments,
        by.x = "catchment_code",
        by.y = "cod_cuenca"
        ) %>% 
  mutate(short_gauge_name = short_river_name(gauge_name))

saveRDS(data_input,paste0("data_output/scores/RDS/model_results_",today(),".RDS"))



if (F) {
  dt = readRDS("data_output/scores/RDS/model_results_2023-01-09.RDS") %>% data.table()
  dt = dt[, c("var", "fun", "horizon") := tstrsplit(predictor_list, "_", fixed = TRUE, keep = 1:3)]
  data_input = dt

  # x-axis order
data_input$month_initialisation = factor(
  data_input$month_initialisation,
  levels = months_initialisation
)
# new labels for horizon
data_input$horizon = factor(data_input$horizon)
levels(data_input$horizon) = c("Todos los meses previos","1 mes previo")

p=ggplot(data=data_input,
         mapping =  aes(
           x= month_initialisation,
           y=crpss_climatology,
           col=horizon
           ))+
  geom_boxplot()+
labs(
    title = "CRPSS volumen estacional sep-mar, 49 cuencas, 1981-2020",
    #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
    x = "Mes de emisión",
    y = "CRPSS (climatología)"
  )+
  scale_color_brewer(palette="Set1")+
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 6))

ggplot2::ggsave("data_output/scores/figures/CRPSS_vol_climate_indices.png",
                dpi=500,plot = p, width =  10,height = 6)    


p1 = subset(data_input,horizon == "1 mes previo") %>% 
  ggplot( 
         mapping =  aes(
           x= "",
           y=crpss_climatology,
           col=var
         ))+
  geom_boxplot()+
  facet_wrap(~month_initialisation)+
  labs(
    title = "CRPSS volumen estacional sep-mar, 49 cuencas, 1981-2020",
    #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
    x = "",
    y = "CRPSS (climatología)"
  )+
  theme(legend.position = "bottom")

ggplot2::ggsave("data_output/scores/figures/CRPSS_vol_climate_indices_per_month.png",
                dpi=500,plot = p1, width =  10,height = 6)    


p2=ggplot(data = subset(data_input,horizon == "1 mes previo"), 
          mapping = aes(y =as.character(catchment_code),
                        x = month_initialisation,
                        fill=crpss_climatology))+
  geom_tile()+
  facet_wrap( ~var)+
  scale_fill_viridis_c()+
  labs(
    title = "CRPSS volumen estacional sep-mar, 49 cuencas, 1981-2020",
    #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
    x = "Mes inicialización",
    fill = "CRPSS",
    y = "Código cuenca DGA")+
  theme(legend.position = "right")




ggplot2::ggsave("data_output/scores/figures/CRPSS_vol_climate_indices_heatmap.png",
                dpi=500,plot = p2,
                width =  10,height = 8)    

}
