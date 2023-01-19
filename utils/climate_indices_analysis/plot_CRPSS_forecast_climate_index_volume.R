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
  
  ggplot2::ggsave("data_output/scores/figures/CRPSS_vol_climate_indices_comparacion_horizonte.png",
                  dpi=500,plot = p, width =  10,height = 6)    
  
  plot(p)
  
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
  plot(p1)
  ggplot2::ggsave("data_output/scores/figures/CRPSS_vol_climate_indices_per_month.png",
                  dpi=500,plot = p1, width =  10,height = 6)    
  
  p2=ggplot(data = subset(data_input,horizon == "1 mes previo"), 
            mapping = aes(x = month_initialisation,
                          y = crpss_climatology))+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = 0.5)+
    geom_boxplot()+
    facet_wrap(~var)+
    labs(
      title = "CRPSS volumen estacional sep-mar, 49 cuencas, 1981-2020",
      #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
      x = "",
      y = "CRPSS (climatología)"
    )+
    theme(legend.position = "bottom")
  plot(p2)
  
  ggplot2::ggsave("data_output/scores/figures/CRPSS_vol_per_climate_indices.png",
                  dpi=500,plot = p2, width =  10,height = 6)    
  
  #plot correlation
  p3=ggplot(data = subset(data_input,horizon == "1 mes previo"), 
            mapping = aes(y = floor(gauge_lat),
                          x = crpss_climatology,
                          group = floor(gauge_lat)))+
    geom_boxplot()+
    geom_vline(xintercept = 0)+
    geom_vline(xintercept = 0.5)+
    facet_wrap(~var)+
    labs(
      title = "CRPSS volumen estacional sep-mar, 49 cuencas, 1981-2020",
      #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
      x = "",
      y = "CRPSS (climatología)")+
    theme(legend.position = "bottom")
  
  plot(p3)
  
  ggplot2::ggsave("data_output/scores/figures/CRPSS_vol_climate_indices_per_latitude.png",
                  dpi=500,plot = p3, width =  10,height = 6)    
  
  #heatmap correlation
  
  
  p4=ggplot(data = subset(data_input,horizon == "1 mes previo"), 
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
                  dpi=500,plot = p4,
                  width =  10,height = 8)    
  
}
