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
  rbindlist()

data_best_models$var = factor(data_best_models$var,
                              levels = c("NINO1.2","SOI","STORAGE"),
                              labels =  c("NIÑO1.2","SOI","CHI"))


#sort month names

dataframe = data_best_models %>%
  data.table() %>%
  select(c("date_label","catchment_code","var","percentage","month_initialisation")) %>% 
  dcast.data.table(date_label+catchment_code+month_initialisation~ var, value.var = "percentage") %>% 
  melt.data.table(id.vars = c("date_label","catchment_code","month_initialisation"),
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
  #subset(month_initialisation %in% c(5,7,9))
# Plot the metric using the merged data
#plot <- 
ggplot(data = merged_data2) +
  geom_sf(aes(fill = percentage,geometry = geometry, colour="")) +
  scale_colour_manual(values=NA) +
  scale_fill_viridis_b(na.value = "white",direction = -1)+
  geom_text(aes(x = -70.5, y = -37, label = paste0(num," cuencas")),size=2) +
  facet_grid(var~ date_label ,)+
  labs(
       x = "Longitud",
       y = "Latitud",
       fill = "Importancia (%)",
       title = "Importancia relativa de los predictores",
       col = "Sin importancia")+
  #coord_sf(xlim = c(-69, -74), ylim = c(-27, -37))+
  theme_void()+
  #  guides(
  #fill = guide_legend(
  #    label.position = "bottom",
  #    #title = "Emisión",
      #title.vjust = 0.8
  #)
  #)+
  theme(
    legend.position = "bottom",
    legend.title.align = 0
    #legend.key.width = unit(0.1,"in")
  )+
  #theme(legend.spacing.x = unit(.2, 'cm'))+
  guides(colour=guide_legend(override.aes=list(fill="white"),title.vjust = 0))


ggsave("data_output/figuras/importancia_predictores/importancia_predictores_geo_v3.png",
       width = 6,height = 6,dpi = 400)



# Add the color for the "STORAGE" predictor and create the color_palette
color_palette <- c("NIÑO1.2" =  "#3B9AB2",
                   "SOI" =  "#E1AF00",
                   "CHI" = "#ff6d5c")

dataframe$month_initialisation = factor(dataframe$month_initialisation, levels = c(seq(4,12),seq(1,3)))

ggplot(data = dataframe, aes(x = month_initialisation, y = percentage,fill = var))+
  geom_bar(stat = "identity", position = "stack")+
  #geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3)+
  labs(x = "Mes de emisión",
       y = "Importancia del predictor",
       fill = "Variable",
       title = paste0("Importancia relativa de los predictor según criterio Akaike (AIC)"),
       #subtitle = catchment_name
       ) +
  scale_fill_manual(values = color_palette) +
  scale_x_discrete( expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  theme_minimal()+
  theme(
    legend.position = "bottom",
    legend.key.width = unit(0.1,"in")
  )+
  facet_wrap(~catchment_code)

ggsave("data_output/figuras/importancia_predictores/importancia_predictores_todascuencas.png",
       width = 10,height = 10,dpi = 400)
