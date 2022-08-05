rm(list = ls())
library(dplyr)
library(ggplot2)

data_input = readRDS("model_results.RDS")
months_initialisation = c('may','jun','jul','ago','sep','oct','nov','dic','ene','feb')

data_input$month_initialisation = factor(
  data_input$month_initialisation,
  levels = months_initialisation
)

# filtrar para una cuenca


ggplot(data=data_input)+
  geom_line(
    aes(
      x=month_initialisation,
      y=crpss_climatology,
      col=region,
      group=region
      )
    )+
  facet_wrap(~catchment_code)+
  theme(legend.position = "bottom")
  #scale_color_viridis_b()
