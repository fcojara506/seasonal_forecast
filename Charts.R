
### plot predictors vs volume
plot_X_y_train <- function(
    data,
    export = FALSE,
    show_chart = FALSE) {
  
  if (show_chart) {
    
    library(ggplot2)
    library(ggpmisc)
    library(broom)
    library(glue)
    library(ggpubr)
    
    # format X_train and y_train
    X_train_df= lapply(
      data$X_train, 
      function(x) rownames_to_column(data.frame(x),var = "wy_simple")) %>% 
      rbindlist(idcol = "ens")
    
    y_train_df = data$y_train %>% 
      rownames_to_column(var = "wy_simple")
    
    # merge X_train and y_train by water year
    train_data_df = merge(X_train_df,
                          y_train_df, 
                          by="wy_simple",
                          all=TRUE) %>% 
      reshape2::melt(id.vars = c("wy_simple","volume_mm","ens")) %>% 
      mutate(wy_simple = as.numeric(wy_simple))
    
    test_data_df = lapply(
      data$X_test, 
      function(x) rownames_to_column(data.frame(x),var = "wy_simple")) %>% 
      rbindlist(idcol = "ens") %>% 
      reshape2::melt(id.vars = c("wy_simple","ens"))
    # test stats min max
    test_data_df_stats =  
      aggregate(value ~ variable,
                data = test_data_df, 
                FUN = function(x) c( min=min(x), max=max(x) ) )
    ## add historical predictors
    
    p=ggplot(
      data = train_data_df,
             aes(
               x = value,
               y = volume_mm,
               col = wy_simple)
      )+
      geom_point()+
      facet_wrap(~variable, scales = "free_x")+
      scale_color_viridis_b()
   
    ## add smooth line and metrics
    p = p +
      geom_smooth(formula = y ~ x,
                  fullrange=T,
                  method = "loess",
                  se = F ) +
      stat_regline_equation(
        aes(label = ..eq.label..),
        size=3,
        label.x.npc = 0.3,
        label.y.npc = 0.11
        ) +
      stat_regline_equation(
        aes(label =..rr.label..),
        size = 3,
        label.x.npc = 0.3,
        label.y.npc = 0.2
        )+
      stat_fit_glance(method = "lm",
                      label.y = "bottom",
                      label.x = 0.73,
                      mapping = aes(label = 
                                      sprintf('P-value = %.1e',
                                                     after_stat(p.value))
                                    ),
                      size=3,
                      parse = F)
    

    ## add test/current predictors


    
    p = p +
      geom_vline(data =  test_data_df_stats,
                 aes(xintercept = value[,"min"]),
                 size = 0.3)+
      geom_vline(data =  test_data_df_stats,
                 aes(xintercept = value[,"max"]),
                 size = 0.3)



    
    ## labels
    p = p + 
      labs(
        title = "Volumen vs Predictores" ,
        subtitle = paste("Cuenca:",data$raw_data$attributes_catchment$gauge_name),
        x= "predictor",
        y= glue("Volumen observado (mm) ", data$plot_text$volume_span_text),
        col="Año hidrológico",
        caption = glue("Emisión {data$plot_text$date_initialisation}")
      )+ theme(
        legend.position = "bottom",
        legend.key.width = unit(0.5,"in")
      )+ 
      expand_limits(y=0)
    
    
    # add vertical text for the target year
    p=p+
      geom_label(
        data = test_data_df_stats,
        aes(x = value[,"max"]-(value[,"max"]-value[,"min"])/2 ),
        y= max(train_data_df$volume_mm)*0.95,
        label= paste("predictor \n wy",data$wy_holdout),
        col='black',
        label.padding = unit(0.1, "lines"),
        size=3
      )
  
  
  p=p+
    geom_label(
      aes(x = Inf),
      y = data$y_test$volume_mm,
      label= paste("volumen \n wy",data$wy_holdout),
      col='black',
      label.padding = unit(0.1, "lines"),
      size=3,
      hjust   = 1
    )+
    geom_hline(yintercept = data$y_test$volume_mm)
  
    plot(p)
    
    return(p)
    
  }
}

plot_vol_sim_obs <- function(
    data_fore,
    data,
    export = FALSE,
    show_chart=FALSE) {
  
  if (show_chart) {
  
  library(ggplot2)
  library(ggpmisc)
  library(broom)
  library(glue)
  
  y_true = data$y_train
    
  y_df = 
  lapply(
    data_fore$y_cv, 
    function(x) 
      
      data.frame(y_sim = x,y_true) %>%
      rownames_to_column(var = "wy") %>% 
      mutate(wy = as.numeric(wy)) %>% 
      rename(y_true = volume_mm)
      
      ) %>% 
    rbindlist(idcol = "ens")
  
  v_line = t(as.data.frame(data_fore$y_fore)) %>%
    data.frame(y_fore=.)
  
  p=
    ggplot(data = y_df, aes(x=y_sim,y=y_true,col=wy))+
    geom_point()+
    geom_abline(slope = 1)+
    geom_jitter(data = v_line, aes(x=y_fore,y=data$y_test$volume_mm), col="red")+
    geom_vline(xintercept =  mean(v_line$y_fore))+
  scale_color_viridis_b()+
    labs(
      x = "Volumen simulado (mm)",
      y = "Volumen observado (mm)",
      title = glue("Volumen observado vs simulado {data$plot_text$volume_span_text}"),
      subtitle = paste("Cuenca:", data$raw_data$attributes_catchment$gauge_name),
      col = "Año Hidrológico",
      caption = glue("Emisión {data$plot_text$date_initialisation}")
    )+ theme(
      legend.position = "bottom",
      legend.key.width = unit(0.5,"in"),
      aspect.ratio=1
    )+
    coord_equal()+
    expand_limits(y=0,x=0)
  
  
  
  p = p +
    stat_poly_eq(aes(label = paste(..rr.label..)),
                 label.x.npc = 0.95,
                 label.y.npc = 0.1,
                 formula = y ~ x,
                 parse = TRUE,
                 size = 3)+
    stat_fit_glance(method = "lm",
                    label.y = "bottom",
                    label.x = 0.95,
                    method.args = list(formula = y ~ x),
                    mapping = aes(
                      label = (sprintf('italic(P-value)~"="~%.2g',
                                                   after_stat(p.value)))),
                    size=3,
                    parse = TRUE)

  # add vertical's text for the target year
  p=p+
    geom_label(
             label= paste("wy",data$wy_holdout),
             x=mean(v_line$y_fore),
             y=max(y_df$y_true),
             col='black'
             )
  
  # add identity function text
  p = p+
    geom_label(
      label= " y = x",
      x=min(y_df$y_sim)*0.2,
      y=min(y_df$y_sim)*0.2,
      col='black'
    )
  # # add coefficients
  # coeff = data.frame(
  #   val=round(coef(data_fore$regression_model$finalModel),2)) %>% 
  #   rownames_to_column(var = "coef")
  # 
  # coeff2 = data.frame(summary(data_fore$regression_model)$coef) %>%
  #   rownames_to_column(var = "coef") %>% 
  #   select(coef,Estimate,Std..Error,Pr...t..) %>% 
  #   rename(coef_val = Estimate) %>% 
  #   rename(std_error=Std..Error) %>% 
  #   rename(p_value = Pr...t..) %>% 
  #   mutate(coef_val = round(coef_val,2)) %>% 
  #   mutate(std_error = round(std_error,1)) %>% 
  #   mutate(p_value = sprintf("%.2g",p_value))
  # 
  # p2=ggplot(data=data.frame(x=10,y=10))+
  #   annotate(
  #     geom = 'table',
  #     x=0,
  #     y=0,
  #     vjust = -0.05,
  #     hjust = 0.6,
  #     label=list(coeff2))+
  #   theme_void()
  
  #library(patchwork)
  
  #p3=p
    #plot_layout(ncol = 1)
    #plot_annotation(
    #  tag_levels = "a",
    #  tag_suffix = ') '
    #  )
  
  plot(p)
return(p)
}
}

#### ensemble volume
vol_subplot <- function(
    y_ens,
    df_train,
    xlabel,
    xticks_colours,
    quantiles_obs
    ) {
  
  library(ggplot2)
  library(grid)
  library(ggtext)

  ## add observed quantiles
  quantiles_text = quantiles_obs %>%
    rownames_to_column(var = "quantiles")
  
  x_labels = unique(y_ens$wy_simple)
  x_limits = length(x_labels)
  
  # plot ensembles
  p1 = ggplot() + 
    geom_violin(data = y_ens,
                 aes(
                   x=wy_simple,
                   y=volume,
                   fill = error_median,
                   #fill = volume#as.numeric(as.character(wy_simple))
                   ),
                 width=0.7,
                 color="black",
                 outlier.shape = NA,
                 lwd=0.1)+
    scale_fill_gradient2(
      low="red",
      mid = "white",
      high = "blue",
      na.value=NA)
  
  # change labels
  p2 = p1+
    theme_light()+
    labs(y = "Volumen (mm)",
         x = xlabel,
         fill = "Sesgo mediana vs obs"
         )+
    theme(
      axis.text.x=element_markdown(angle=90,
                                   hjust=3,
                                   colour = xticks_colours),
      legend.background = element_blank()
    )+
    ylim(0,NA)
  
  ## add observed data
  p3 = p2 +
    geom_point(data = df_train,
               aes(
                 x = wy_simple,
                 y = obs,
                 col="Observaciones"
                 ),
               shape=4,
               size = 1
    )+
    scale_color_manual(values = c("Observaciones" = "black"),name="")
  
  
  # add observed quantiles 
  p4 = p3 +
    theme(plot.margin = unit(c(1,5,1,0.5), "lines"))+
    geom_text(data = quantiles_text,
              aes(x = x_limits+2,
                  y=quantiles_obs,
                  label = glue("{quantiles} ({round(quantiles_obs,1)} mm)")),
              vjust = 0.4,hjust = 0,
              size=2)+
    coord_cartesian(xlim = c(0, x_limits+1), clip = "off")
  
  
  p5 = p4 +
    geom_segment(data = quantiles_text,
                 aes(x = 1,
                     xend = x_limits,
                     y=quantiles_obs,
                     yend = quantiles_obs),
                 linetype="dashed")
  p6 = p5+
    theme(legend.position="bottom")
  return(p6)
}

data_plot_backtest_volume <- function(data,data_fore) {
  library(glue)
  y_ens_cv = data_fore$y_ens_cv
  y_ens_fore = data_fore$y_ens_fore
  y_train = data$y_train$volume_mm
  wy_train = data$wy_train
  wy_holdout = data$wy_holdout
  
  # quantiles observations and ensemble forecast of hold-out year
  quantiles_obs = quantile(y_train, probs = c(0.05, 0.5, 0.95) )
  quantiles_fore = quantile(y_ens_fore, probs = c(0.25, 0.5, 0.75) )
  quantile_target = ecdf(y_train)
  
  # plot(quantile_target,
  #      xlab='Volume (mm)',
  #      ylab='CDF',
  #      main='CDF del volumen',
  #      verticals = FALSE,
  #      col.points = "blue",
  #      col.hor = "red",
  #      col.vert = "bisque"
  #      )
  #abline(v=median(y_ens_fore))
  #abline(h=quantile_target(median(y_ens_fore)))
  
  
  # compute uncertainty error as max between the median and the interquantile limits
  error_range_fore = (quantiles_fore - median(y_ens_fore))
  error_range_fore = max(abs(error_range_fore[error_range_fore != 0]))
  
  # median and range into string
  median = sprintf("%0.1f", median(y_ens_fore))
  error = sprintf("%0.1f", error_range_fore)
  text_forecast_range = glue( '{median} ± {error} mm')
  
  # create dataframe to insert data in charts
  # water years dataframe
  
  y_ens = cbind(y_ens_cv,y_ens_fore)
  y_ens = y_ens[ , order(colnames(y_ens))] %>% data.table()
  y_ens = melt.data.table(y_ens,
                          variable.name = "wy_simple",
                          value.name = "volume",
                          measure.vars = colnames(y_ens),
                          variable.factor = F)
  
  # observations (training data) data frame
  df_train = cbind(wy_train, y_train)
  colnames(df_train) = c("wy_simple","obs")
  df_train = rbind(df_train, data.frame('wy_simple'= wy_holdout, 'obs'= NaN))
  df_train = df_train[order(df_train$wy_simple),]
  df_train$wy_simple = as.character(df_train$wy_simple)
  
  
  return(list(
    df_train = df_train,
    y_ens = data.frame(y_ens,check.names = F),
    text_forecast_range = text_forecast_range,
    quantiles_obs = data.frame(quantiles_obs),
    quantile_target = quantile_target
  ))
}

plot_backtest_volume <- function(
    data,
    data_fore,
    subplot = TRUE,
    export = FALSE,
    show_chart=TRUE) {
  
  
  plot_data = data_plot_backtest_volume(
    data = data,
    data_fore = data_fore)
  
  plot_text = data$plot_text
  
  y_ens = plot_data$y_ens
  
  quantiles_obs = plot_data$quantiles_obs
  
  # median
  medians_forecast = aggregate(volume ~ wy_simple,
                               data = y_ens,
                               FUN = median)
  
  df_train = plot_data$df_train %>%
    merge(medians_forecast) %>%
    rename(sim_median = volume) %>% 
    mutate(error_median = round((obs-sim_median)/obs*100,2))
  
  
  y_ens = merge(y_ens,df_train)
  
  xticks_colours  <- unique(y_ens$wy_simple) %>%
    {(.== data$wy_holdout)} %>%
    ifelse("red","black")
  

  title =  glue("Pronóstico retrospectivo de volumen {plot_text$volume_span_text}")
  subcaption =glue("Pronóstico de volumen {plot_text$volume_span_text_v2}: {plot_data$text_forecast_range} (mediana ± rango intercuartil/2)\nEmisión {data$plot_text$date_initialisation}")
  
  p = vol_subplot(
    y_ens = y_ens,
    df_train = df_train,
    xlabel = "Año hidrológico (orden cronológico)",
    xticks_colours = xticks_colours,
    quantiles_obs = quantiles_obs
    )
  
  
  
  
    p1 = p
  
 
    # compute new order of x-axis based on median of the forecast

    medians_forecast_order = arrange(medians_forecast,volume)
    y_ens$wy_simple <- factor(y_ens$wy_simple , levels=medians_forecast_order$wy_simple)
    
    xticks_colours  <- unique(medians_forecast_order$wy_simple) %>%
      {(.== data$ wy_holdout)} %>%
      ifelse("red","black")
    
  
    
    p2 = vol_subplot(y_ens = y_ens,
                        df_train = df_train,
                        xlabel =  "Año hidrológico (orden por mediana del pronóstico)",
                        xticks_colours = xticks_colours,
                        quantiles_obs = quantiles_obs)
    
    
        
      if (subplot) {
    #p1 = p1 + labs(title = title)
    p2 = p2 + 
      labs(caption  = subcaption)+
      theme(plot.caption = element_text(hjust = 0))
    
    library(patchwork)
    p3=p1/p2 + 
      plot_annotation(
        title = title,
        subtitle = data$raw_data$attributes_catchment$gauge_name,
        tag_levels = "a"
        )+
      plot_layout(
        ncol = 1,
        guides = "collect"
        ) & 
      theme(legend.position = 'bottom')

    #plot(p3)
    
    # Figure size
    width_p = 7.2
    height_p = 6
  }else{
    p3 = p2 + 
      labs(
        title = title,
        subtitle = data$raw_data$attributes_catchment$gauge_name,
        caption  = subcaption)+
      theme(plot.caption = element_text(hjust = 0)
            )
    # figure
    width_p = 7.2
    height_p = 4
  }
  
  ## exportar
  if (export) {
    library("icesTAF")
    # figure output folder
    folder_output = glue("data_output/pronostico_volumen/Figures/ensemble_forecast/{data$args$catchment_code}/")
    # create folder if it does not exist
    mkdir(folder_output)
    # filename
    figure_vol_output = glue(
      "{folder_output}EnsembleVolumeHindcast_{data$args$catchment_code}_",
      "1st{data$args$month_initialisation}_{plot_text$predictor_list_join}_",
      "{plot_text$volume_span_text}{data$wy_holdout}.png")
    
    ggsave(figure_vol_output,plot = p3, width = width_p, height = height_p)
  }
  
  if (show_chart) {
    plot(p3)
    return(p3)
  }
    
}


### monthly stream flows from knn

data_plot_knn_flow <- function(data,q_fore) {
  
  months_wy_forecast = colnames(q_fore)
  months_wy_df = data.frame(wym_str = months_wy) %>% 
    mutate(wym = row_number())
  # ensemble flow forecast
  q_fore = data.table(q_fore) %>%
    rowid_to_column("ens")
  
  q_ens = melt.data.table(q_fore,
                          variable.name = "wym_str",
                          variable.factor = F,
                          measure.vars = months_wy_forecast,
                          id.vars = "ens"
                          )
  
  q_ens = merge.data.table(q_ens,
                           months_wy_df,
                           by = "wym_str",
                           all.y = T) %>% 
    setkey(wym)
  
  q_ens$wym_str <- factor(q_ens$wym_str , levels=data$time_horizon$months_wy)
  # flow observations
  q_flows = data$raw_data$monthly_flows[,c("wy_simple", "wym", "Q_mm")]
  
  # flow observations only water year hold.out (if exists)
  q = subset(q_flows, wy_simple == data$wy_holdout)[,c("wym", "Q_mm")] %>%
    mutate(variable = 'wy_holdout')
  
  colnames(q) = c("wym","value","variable")
  
  # flow observations
  FUN = function(x){c(mean = mean(x), len = median(x))}
  
  q_obs_stats = subset(q_flows, wy_simple != data$wy_holdout)[,c("wym", "Q_mm")] %>%
    group_by(wym) %>%
    summarise(mean=mean(Q_mm),
              median = median(Q_mm),
              percentile_5 = quantile(Q_mm,0.1),
              percentile_95 = quantile(Q_mm,0.9)) %>%
    data.table() %>%
    melt.data.table(id.vars = "wym")
  
  # merge observation data
  q_obs = rbind(q,q_obs_stats) %>%
    mutate(wym_str = data$time_horizon$months_wy[wym])
  ## text
  library(glue)
  wy_init = min(data$wy_train)
  wy_end = max(data$wy_train)
  
  legend_labels = 
    c(glue('Medido/Natural ({data$wy_holdout})'),
      glue('Promedio [{wy_init},{wy_end}]'),
      glue('Mediana [{wy_init},{wy_end}]'),
      glue('Percentil 10% [{wy_init},{wy_end}]'),
      glue('Percentil 90% [{wy_init},{wy_end}]'))
  
  
  return(
    list(
      q_ens = q_ens,
      q_obs = q_obs,
      legend_labels = legend_labels
    )
  )
}


plot_knn_flow <- function(
    data,
    q_fore,
    export = FALSE,
    show_chart = FALSE) {
  
  library(ggplot2)
  plot_text = data$plot_text
  q_plot_data = data_plot_knn_flow(data = data,q_fore=q_fore)
  
  
  p=ggplot(
    data = q_plot_data$q_ens,
    mapping =  aes(x=wym_str,y=value)
    )+
    #geom_line(aes(group=ens))
    #geom_jitter()+
    geom_violin(
      draw_quantiles = c(0.25, 0.5, 0.75)
      #color='skyblue'
      )
    
  # add observations
  p = p + 
    geom_line(
      data = q_plot_data$q_obs,
      aes(x=wym_str,y = value,color = variable, group=variable)
    )+
    geom_point(
      data = q_plot_data$q_obs,
      aes(x=wym_str,y = value,color = variable, group=variable)
    )
  # add aestetics
  p = p + scale_color_discrete(
    name = "Caudal Estación Fluviométrica",
    labels = q_plot_data$legend_labels
  )+
    labs(x= ("Periodo del pronóstico"),
         y = ("Caudal medio mensual (mm)"),
         title=glue("Caudal medio mensual pronosticado"),
         subtitle = data$attributes_catchment$gauge_name,
         caption = glue("Emisión {plot_text$date_initialisation}")
         )+
    theme(legend.position = 'bottom')+
    guides(color=guide_legend(nrow=3,byrow=F))
  
  # idea add an inset plot with forecast period
  #library(patchwork)
  # p1 = p +
  #   scale_x_discrete(limits=data$time_horizon$months_forecast_period,
  #                    expand = c(0,0))+
  #   theme(legend.position = "")+
  #   labs(x="",y="",title="")+
  #   theme(rect = element_rect(fill = "transparent"))
  # 
  # p2 = p +
  #   inset_element(p1,
  #                 left = -0.02,
  #                 bottom = 0.3,
  #                 right = 0.5,
  #                 top = 1.1)
  
  #plot(p2)
  
  if (export) {
    library("icesTAF")
    # figure output folder
    folder_output = glue("data_output/pronostico_caudal/Figures/ensemble_forecast/{data$args$catchment_code}/")
    # create folder if it does not exist
    mkdir(folder_output)
    width_p = 7.2
    height_p = 4
    #filename
    figure_q_output = glue(
      "{folder_output}flow_ensemble_forecast_{data$args$catchment_code}_",
      "1st{data$args$month_initialisation}_{plot_text$predictor_list_join}_",
      "{plot_text$volume_span_text}{data$wy_holdout}.png")
    
    ggsave(figure_q_output,plot = p2, width = width_p, height = height_p)
  }
  if (show_chart) {
    plot(p)
    return(p)
  }
  
  
}



