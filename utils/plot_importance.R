rm(list = ls())
library(data.table)
library(tidyr)


#all available catchments, no data 6008005, 7317005, 7355002, 8106001
catchments_attributes_filename = "data_input/attributes/attributes_49catchments_ChileCentral.csv" 
attributes_catchments = read.csv(catchments_attributes_filename)[-c(32,40,45,49),]
cod_cuencas = attributes_catchments$cod_cuenca

data_best_models = lapply(cod_cuencas, function(x)
  readRDS(file = paste0("data_output/mejores_modelos_cuenca_mes/",x,"_may-mar.RDS"))$importance) %>% 
  rbindlist()

#sort month names
months_wy <- c("abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic", "ene", "feb", "mar")
data_best_models$month_wy = factor(data_best_models$month_wy, levels = paste0("1˚",months_wy) )


dataframe = data_best_models %>%
  data.table() %>%
  select(c("date_label","catchment_code","var","percentage")) %>% 
  dcast(date_label+catchment_code~ var, value.var = "percentage") %>% 
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



# Plot the metric using the merged data
#plot <- 
ggplot() +
  geom_sf(data = merged_data, aes(fill = percentage, colour="")) +

  scale_colour_manual(values=NA) + 
  scale_fill_continuous(low = "white", high = "#6495ED",na.value="red") + # Change the colors according to your preference
  #scale_color_manual(values = 'red', labels = 'Missing value') +
  facet_grid(var ~ date_label)+
  #facet_grid( date_label ~ var)+
  scale_x_continuous(breaks = seq(-68,-74,by = -4),
                     labels = seq(-68,-74,by = -4)) +
  #scale_y_continuous(breaks = seq(-27, -37, by = -2),labels = seq(-27, -37, by = -2))+
  labs(
       x = "Longitud",
       y = "Latitud",
       fill = "Importancia (%)",
       title = "Importancia de los predictores por fecha de emisión")+
  coord_sf(xlim = c(-68, -74), ylim = c(-27, -37))+
  theme(legend.position = "bottom",
        legend.spacing.x = unit(0, 'cm'))+
  guides(colour=guide_legend("No relevancia", override.aes=list(colour="red")))

ggsave("data_output/figuras/importancia_predictores/importancia_predictores_geo.png",
       width = 8,height = 7)
