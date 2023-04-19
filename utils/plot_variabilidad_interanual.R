rm(list = ls())
library(ggplot2)
library(ggrepel)
library(dplyr)

source("base/Preprocess_data.R")

catchments_attributes_filename = "data_input/attributes/attributes_49catchments_ChileCentral.csv" 

cod_cuencas = fread(catchments_attributes_filename) %>%
  #no hydrological model data
  subset(!(cod_cuenca %in% c(6008005, 7317005, 7355002, 8106001))) %>%
  #bad scores
  #subset(!(cod_cuenca %in% c(7381001,4531002,4522002,4515002))) %>% 
  select(cod_cuenca) %>% unlist()

forecast_mode = "cv"
month_initialisation = 9
for (catchment_code in cod_cuencas) {
  

data_input <- preprocess_data(
  datetime_initialisation = lubridate::make_date(2022, month_initialisation),
  forecast_mode = forecast_mode,
  catchment_code = catchment_code,
  predictor_list = c("pr_sum_-1months","tem_mean_3months","STORAGE_mean_1months"),
  save_raw = T
)
## meteo
x_meteo = data_input$raw_data$monthly_meteo %>%
  subset(wy_simple %in% seq(1981,2019)) %>% 
  melt.data.frame(id.vars = c("wy_simple","wym"))

x_meteo_avg = 
  aggregate(
  x = value ~ wym+variable ,
  data = x_meteo,
  FUN = median
) %>% 
  data.table() %>% 
  dcast.data.table(formula = wym ~ variable,value.var = "value") %>% 
  mutate(wym_str = (data_input$time_horizon$months_wy[[1]][wym]))
x_meteo_avg$wym_str = factor(x_meteo_avg$wym_str,
                              levels=unlist(data_input$time_horizon$months_wy))
                             

#caudal
x_caudal = data_input$raw_data$monthly_flows %>% 
  subset(wy_simple %in% seq(1981,2019)) %>% 
  select(-Q_converted)

x_caudal_avg = 
  aggregate(
    x = Q_mm ~ wym ,
    data = x_caudal,
    FUN = median
  ) %>% 
  mutate(wym_str = data_input$time_horizon$months_wy[[1]][wym])
x_caudal_avg$wym_str = factor(x_caudal_avg$wym_str,
                              levels=unlist(data_input$time_horizon$months_wy))

factor_b = diff(range(x_meteo_avg$pr))/diff(range(x_meteo_avg$tem))*0.8

p = ggplot() +
  #precipitacion
  geom_bar(data = x_meteo_avg,aes(x = wym_str, y = pr),
           stat="identity", width = 0.75,fill = "lightblue")+
  scale_y_continuous(expand = c(0,0))+
  ylim(0,NA)+
  geom_label(data = mutate(x_meteo_avg,label = "pr"),
             aes(x = wym_str, y = pr,label = label),fill = "lightblue",
             hjust=0.5, vjust=0,size = 2,label.padding = unit(0.15, "lines"))+
  # temperatura
  geom_point(data = x_meteo_avg,aes(x = wym_str, y = tem*factor_b),color = "orange")+
  geom_label(data = mutate(x_meteo_avg,label = "tem"),
             aes(x = wym_str, y = tem*factor_b,label = label), fill = "orange",
             hjust=0.5, vjust=0,size = 2,label.padding = unit(0.15, "lines"))+
  # doble eje
  scale_y_continuous("precipitación o caudal (mm)",
                   sec.axis = sec_axis(trans= ~./10,
                                       name = "Temperatura (˚C)"))+
  # caudal  
  geom_line(data = mutate(x_caudal_avg,label = "caudal"),
            aes(x = wym_str, y = Q_mm,group = ""))+
  geom_label(data = mutate(x_caudal_avg,label = "caudal"),
            aes(x = wym_str, y = Q_mm,label = label),
            hjust=0.5, vjust=0,size = 2,label.padding = unit(0.15, "lines"))+
  labs(
    title = "Variabilidad interanual promedio 1981-2019",
    subtitle = paste(data_input$raw_data$attributes_catchment$gauge_name),
    x = "")+
  scale_x_discrete(expand = c(0,0))
  
  
ggsave(
  glue(
    "data_output/presentaciones/variabilidad_interanual_{data_input$info$catchment_code}.png"
  ),
  plot = p,
  width = 6,
  height = 3,
  dpi = 300
)
}

