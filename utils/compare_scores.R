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
  paste(climate_indices,"mean",paste0(1,"months"),sep = "_"),
  paste(climate_indices,"mean",paste0(-1,"months"),sep = "_")
)

library(foreach)
library(doParallel)
#registerDoParallel(cores=8)

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
      test_subset = T
    )
    
  } %>% purrr::transpose()


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
  
# x-axis order
data_input$month_initialisation = factor(
  data_input$month_initialisation,
  levels = months_initialisation
)


p=ggplot(data=data_input)+
  geom_boxplot(
    aes(
      x= month_initialisation,
      y=crpss_climatology,
      col=direction
      #group=direction
    )
  )+
  labs(
    title = "CRPSS seasonal volume, 49 catchments, 1981-2019",
    subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
    x = "Mes de emisión",
    y = "CRPSS (climatology)",
    col = ""
  )+
  scale_color_brewer(palette="Set1")+
  #facet_wrap(~catchment_code)+
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 6))
#scale_x_discrete(labels = months_initialisation_initials)
print(p)
ggplot2::ggsave("data_output/scores/figures/CRPSS_seasonal_volume_direction_flow_or_vol.png",dpi=500,plot = p)    
}
