rm(list = ls())
library(data.table)
library(tidyr)


#all available catchments, no data 6008005, 7317005, 7355002, 8106001
catchments_attributes_filename = "data_input/attributes/attributes_49catchments_ChileCentral.csv" 

cod_cuencas = fread(catchments_attributes_filename) %>%
  subset(!(cod_cuenca %in% c(6008005, 7317005, 7355002, 8106001))) %>% 
  select(cod_cuenca) %>% unlist()

data_best_models = lapply(cod_cuencas, function(x)
  readRDS(file = paste0("data_output/mejores_modelos_cuenca_mes/",x,"_may-mar.RDS"))$importance) %>% 
  rbindlist() %>% 
  subset(month_initialisation %in% seq(5,9))

#sort month names

dataframe = data_best_models %>%
  data.table() %>%
  select(c("date_label","catchment_code","var","percentage")) %>% 
  dcast.data.table(date_label+catchment_code~ var, value.var = "percentage") %>% 
  melt.data.table(id.vars = c("date_label","catchment_code"),
       variable.name  = "var",
       value.name = "percentage"
       ) #%>% zoo::na.fill0(0)

library("ggplot2")
library("sf")
shapefile_path = "data_input/SIG/shapefile_cuencas/cuencas_fondef-dga.shp"
# Read the shapefile
shapefile <- st_read(shapefile_path,quiet = T)
shapefile <- st_make_valid(shapefile)
shapefile <- st_simplify(shapefile, dTolerance = 4000)

# Merge the shapefile and the dataframe using the common ID
merged_data <- merge(shapefile,
                     dataframe,
                     by.x = "gauge_id",
                     by.y = "catchment_code",) 


a = merged_data %>%
  select(percentage,var,date_label) %>% 
  group_by(var,date_label) %>% 
  reframe(
    num = length(percentage[!is.na(percentage)])
    )
    
merged_data2 = merge(a,merged_data)
# Plot the metric using the merged data
#plot <- 
ggplot(data = merged_data2) +
  geom_sf(aes(fill = percentage,geometry = geometry, colour="")) +
  scale_colour_manual(values=NA) +
  scale_fill_viridis_b(na.value = "#d9c7af",direction = -1)+
  geom_text(aes(x = -70.5, y = -37, label = paste0(num," cuencas")),size=2) +
  facet_grid(var ~ date_label,shrink = T)+
  labs(
       x = "Longitud",
       y = "Latitud",
       fill = "Importancia (%)",
       title = "Importancia relativa de los predictores",
       col = "Sin importancia")+
 # coord_sf(xlim = c(-69, -74), ylim = c(-27, -37))+
  theme_void()+
  theme(legend.spacing.x = unit(.2, 'cm'))+
  guides(colour=guide_legend(override.aes=list(fill="#d9c7af")))

ggsave("data_output/figuras/importancia_predictores/importancia_predictores_geo_v2.png",
       width = 4,height = 5,dpi = 400)
