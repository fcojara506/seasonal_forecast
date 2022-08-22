rm(list = ls())
library(dplyr)
library(ggplot2)

short_river_name <- function(var) {stringr::word(var,start = 1,end = 2)}
months_initialisation = c('may','jun','jul','ago','sep','oct','nov','dic','ene','feb')
months_initialisation_initials = sapply(months_initialisation, function(x) toupper(substr(x,1,1)))


data_input = readRDS("model_results_v2.RDS") %>% 
  mutate(short_gauge_name = short_river_name(gauge_name))

# x-axis order
data_input$month_initialisation = factor(
  data_input$month_initialisation,
  levels = months_initialisation
)


# filtrar para una cuenca


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
