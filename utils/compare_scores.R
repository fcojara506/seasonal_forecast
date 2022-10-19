rm(list = ls())

source("utils/run_model_function.R")
#test catchments
catchments_attributes_filename = "data_input/attributes_49catchments_ChileCentral.feather"
attributes_catchments = feather::read_feather(catchments_attributes_filename)

cod_cuencas = attributes_catchments$cod_cuenca
#test months initial
months_initialisation = c('may','jun','jul','ago','sep','oct','nov','dic','ene','feb')
months_initialisation_initials = sapply(months_initialisation, function(x) toupper(substr(x,1,1)))
#test meteo set 
regions = c(
  #"ChileCentral_CR2MET",
  #"ChileCentral_era5raw",
  #"ChileCentral_era5QDM",
  "ChileCentral_ens30avg"
  )
#test forecast order
directions = c("vol_to_flow")#c("vol_to_flow","flow_to_vol")

iterations = length(cod_cuencas)*length(months_initialisation)*length(regions)*length(directions)
print(iterations)

library(foreach)
library(doParallel)
registerDoParallel(cores=8)

model <-
  foreach(month_initialisation=months_initialisation,.combine = "c") %:%
  foreach(region=regions,.combine = "c") %:%
  foreach(direction=directions,.combine = "c") %:%
  foreach(catchment_code=cod_cuencas) %dopar% {
    
    run_model(
      catchment_code = catchment_code,
      month_initialisation = month_initialisation,
      region = region,
      direction = direction
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

saveRDS(data_input,"model_results_v2.RDS")

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
ggplot2::ggsave("CRPSS_seasonal_volume_direction_flow_or_vol.png",dpi=500)    
}
