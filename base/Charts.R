library(data.table)
library(ggplot2)
library(lubridate)

### plot predictors vs volume
plot_X_y_train <- function(
    data_input,
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
      data_input$X_train, 
      function(x) rownames_to_column(data.frame(x),var = "wy_simple")) %>% 
      rbindlist(idcol = "ens")
    
    y_train_df = data_input$y_train %>% 
      rownames_to_column(var = "wy_simple")
    
    # merge X_train and y_train by water year
    train_data_df = merge(X_train_df,
                          y_train_df, 
                          by="wy_simple",
                          all=TRUE) %>% 
      reshape2::melt(id.vars = c("wy_simple","volume","ens")) %>% 
      mutate(wy_simple = as.numeric(wy_simple))
    
    test_data_df = lapply(
      data_input$X_test, 
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
               y = volume,
               col = wy_simple)
      )+
      geom_point()+
      facet_wrap(~variable, scales = "free_x",nrow = 1)+
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
        subtitle = paste("Cuenca:",data_input$raw_data$attributes_catchment$gauge_name),
        x= "predictor",
        y= glue("Volumen observado ({data_input$info$units_y}) ", data_input$time_horizon$volume_span_text),
        col="Año hidrológico",
        caption = glue("Emisión {data_input$time_horizon$date_initialisation}")
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
        y= max(train_data_df$volume)*0.95,
        label= paste("predictor \n wy",data_input$wy_holdout),
        col='black',
        label.padding = unit(0.1, "lines"),
        size=3
      )
  if (!(is.null(data_input$y_test$volume))) {
    
  p=p+
    geom_label(
      aes(x = Inf),
      y = data_input$y_test$volume,
      label= paste("volumen \n wy",data_input$wy_holdout),
      col='black',
      label.padding = unit(0.1, "lines"),
      size=3,
      hjust   = 1
    )+
    geom_hline(yintercept = data_input$y_test$volume)
  
  }
    plot(p)
    
    return(p)
    
  }
}

# Function to plot the metric
plot_metric <- function(dataframe, metric, shapefile_path = "data_input/SIG/shapefile_cuencas/cuencas_fondef-dga.shp") {
  ## usage
  # dataframe <- best$best_results
  # metric <- "pbias"
  # plot <- plot_metric(dataframe, metric)
  # print(plot)
  
  # Load required libraries
  library(rgdal)
  library(ggplot2)
  library(sf) 
  # Read the shapefile
  shapefile <- st_read(shapefile_path)
  
  # Merge the shapefile and the dataframe using the common ID
  merged_data <- merge(shapefile, dataframe, by.x = "gauge_id", by.y = "catchment_code")
  
  # Plot the metric using the merged data
  plot <- ggplot() +
    geom_sf(data = merged_data, aes(fill = !!sym(metric))) +
    scale_fill_continuous(low = "blue", high = "red") + # Change the colors according to your preference
    facet_wrap(~month_initialisation) +
    labs(title = "", x = "Longitud", y = "Latitud", fill = metric)
  
  return(plot)
}


plot_vol_sim_obs <- function(
    data_fore,
    data_input,
    export = FALSE,
    show_chart=FALSE) {
  
  if (show_chart) {
  
  library(ggplot2)
  library(ggpmisc)
  library(broom)
  library(glue)
  
  y_true = data_input$y_train
  x = data_fore$y_cv[[1]]  
  y_df = 
  lapply(
    data_fore$y_cv, 
    function(x) 
      
      data.frame(y_sim = x,y_true) %>%
      rownames_to_column(var = "wy") %>% 
      mutate(wy = as.numeric(wy)) %>% 
      dplyr::rename(y_true = volume)
      
      ) %>% 
    rbindlist(idcol = "ens")
  
  v_line = t(as.data.frame(data_fore$y_fore)) %>%
    data.frame(y_fore=.)
  #geom_jitter(data = v_line, aes(x=y_fore,y=data_input$y_test$volume), col="red")+ 
p = 
    ggplot(data = y_df, aes(x=y_sim,y=y_true,col=wy))+
    geom_point()+
    geom_abline(slope = 1)+
    geom_vline(xintercept =  mean(v_line$y_fore))+
  scale_color_viridis_b()+
    labs(
      x = glue("Volumen simulado {data_input$time_horizon$volume_span_text} ({data_input$info$units_y})"),
      y = glue("Volumen observado {data_input$time_horizon$volume_span_text} ({data_input$info$units_y})"),
      title = "Volumen observado vs simulado en validación cruzada",
      subtitle = paste("Cuenca:", data_input$raw_data$attributes_catchment$gauge_name),
      col = "Año Hidrológico",
      caption = glue("Emisión {data_input$time_horizon$date_initialisation}")
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
             label= paste("wy",data_input$wy_holdout),
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
  
  if (!(is.null(data_input$y_test$volume))) {
    p =p+geom_jitter(data = v_line, aes(x=y_fore,y=data_input$y_test$volume), col="red")
  }
  
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
  library(see)
  # plot ensembles
  p1 = ggplot() + 
    geom_violinhalf(
      data = y_ens,
      aes(x = wy_simple,
          y = volume,
          fill = error_median
          ),
          #fill = volume#as.numeric(as.character(wy_simple))),
          width = 0.9,
          color = "black",
          lwd = 0.1,
      scale = "width"
      ) +
    geom_boxplot(
      data = y_ens,
      aes(x = wy_simple,
          y = volume,
          fill = error_median),
      color = "black",
      lwd = 0.1,
      width = 0.2,
      outlier.size = 0.5
    )+
        scale_fill_gradient2(
          guide = "legend",
          low = "blue",
          mid = "white",
          high = "red",
          na.value = NA,
          breaks = seq(-100,100,10)
        )
      
  # change labels
  p2 = p1+
    theme_light()+
    labs(y =  glue("Volumen ({data_input$info$units_y})"),
         x = xlabel
         )+
    theme(
      axis.text.x=element_text(
        angle=90,
        vjust = 0.5, hjust=1,
        colour = xticks_colours
        )
    )+
    ylim(0,NA)
  
  ## add observed data
  p3 = p2 +
    geom_point(data = df_train,
               aes(
                 x = wy_simple,
                 y = obs,
                 col=" "
                 ),
               shape = 4,
               size  = 2
    )+
    scale_color_manual(
      values = c(" " = "black"),
      name="Medido/Natural")
  
  
  # add observed quantiles 
  p4 = p3 +
    theme(plot.margin = unit(c(1,5,1,0.5), "lines"))+
    geom_text(data = quantiles_text,
              aes(x = x_limits+2,
                  y = quantiles_obs,
                  label = glue("{quantiles} ({round(quantiles_obs,1)} mm)")),
              vjust = 0.4,
              hjust = 0,
              size=2)+
    coord_cartesian(xlim = c(0, x_limits+1), clip = "off")
  
  
  p5 = p4 +
    geom_segment(data = quantiles_text,
                 aes(x = 1,
                     xend = x_limits,
                     y=quantiles_obs,
                     yend = quantiles_obs),
                 linetype="dashed")
  
  p6 = p5 +
    theme(legend.position="bottom",
          legend.spacing.x = unit(0, 'cm'))+
    guides(
      fill = guide_legend(
        label.position = "bottom",
        nrow = 1,
        title = "Error: (obs-sim)/obs (%)",
        title.vjust = 0.8
      ),
      color=guide_legend(
        title.vjust = 0.8,
        nrow=2
      )
    )
    #theme(legend.title = element_text(size = 5))+
    #theme(legend.text = element_text(size = 5))+
    #theme(legend.position = c(0.2,0.85))
    #      legend.box = "horizontal",
    #      legend.background = element_blank(),
    #      legend.key.size = unit(0.3,"cm"))
    
  
  return(p6)
}

data_plot_backtest_volume <- function(data_input,data_fore) {
  library(glue)
  
  y_ens_cv = data_fore$y_ens_cv
  y_ens_fore = data_fore$y_ens_fore
  y_train = data_input$y_train$volume
  wy_train = data_input$wy_train
  wy_holdout = data_input$wy_holdout
  
  # quantiles observations and ensemble forecast of hold-out year
  quantiles_obs = quantile(y_train, probs = c(0.05, 0.5, 0.95) )
  quantiles_fore = quantile(y_ens_fore, probs = c(0.25, 0.5, 0.75) )
  quantile_target = ecdf(y_train)
  
  # compute uncertainty error as max between the median and the interquantile limits
  error_range_fore = (quantiles_fore - median(y_ens_fore))
  error_range_fore = max(abs(error_range_fore[error_range_fore != 0]))
  
  # median and range into string
  median_vol = sprintf("%0.1f", median(y_ens_fore))
  error_vol = sprintf("%0.1f", error_range_fore)
  text_forecast_range = glue( '{median_vol} ± {error_vol} mm')
  
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
  
  y_ens_medians =
    aggregate(
    formula = volume ~ wy_simple,
    data = y_ens,
    FUN = median
    )
  
  df_train =
    merge(df_train,y_ens_medians) %>%
    rename(sim_median = volume) %>% 
    mutate(error_median = round((obs-sim_median)/obs*100,2))
  
  y_ens = merge(y_ens,df_train)
  
  return(list(
    y_ens = y_ens,
    df_train = df_train,
    y_ens_medians = y_ens_medians,
    text_forecast_range = text_forecast_range,
    quantiles_obs = data.frame(quantiles_obs),
    quantile_target = quantile_target
  ))
}

plot_backtest_volume <- function(
    data_input,
    data_fore,
    subplot = TRUE,
    export = FALSE,
    show_chart=TRUE) {
  
  
  plot_data = data_plot_backtest_volume(
    data_input = data_input,
    data_fore = data_fore)
  
   y_ens = plot_data$y_ens
   df_train = plot_data$df_train
   medians_forecast = plot_data$y_ens_medians
  
  
  ##################### cronological order
  xticks_colours  <- unique(y_ens$wy_simple) %>%
    {(.== data_input$wy_holdout)} %>%
    ifelse("red","black")
  
  p = vol_subplot(
    y_ens = y_ens,
    df_train = df_train,
    xlabel = "Año hidrológico (orden cronológico)",
    xticks_colours = xticks_colours,
    quantiles_obs = plot_data$quantiles_obs
    )
  
    p1 = p
    
    title =  glue("Pronóstico retrospectivo de volumen {data_input$time_horizon$volume_span_text}")
    subcaption =
      glue("Pronóstico de volumen {data_input$time_horizon$volume_span_text_v2}: {plot_data$text_forecast_range} (mediana ± rango intercuartil/2)\n",
      "Emisión {data_input$time_horizon$date_initialisation}")
    
  ##############
  # compute new order of x-axis based on median of the forecast

    medians_forecast_order = arrange(medians_forecast,volume)
    y_ens$wy_simple <- factor(y_ens$wy_simple , levels=medians_forecast_order$wy_simple)
    
    xticks_colours  <- unique(medians_forecast_order$wy_simple) %>%
      {(.== data_input$ wy_holdout)} %>%
      ifelse("red","black")
    
    p2 = vol_subplot(y_ens = y_ens,
                        df_train = df_train,
                        xlabel =  "Año hidrológico (orden por mediana del pronóstico)",
                        xticks_colours = xticks_colours,
                        quantiles_obs = plot_data$quantiles_obs)
    
      if (subplot) {
    
    p2 = p2 + 
      labs(caption  = subcaption)+
      theme(plot.caption = element_text(hjust = 0))
    
    library(patchwork)
    p3 = (p1/p2) + 
      plot_annotation(
        title = title,
        subtitle = data_input$raw_data$attributes_catchment$gauge_name,
        tag_levels = "a"
        )+
      plot_layout(
        #ncol = 1,
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
        subtitle = data_input$raw_data$attributes_catchment$gauge_name,
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
    folder_output = glue("data_output/pronostico_volumen/Figures/ensemble_forecast/{data_input$info$catchment_code}/")
    # create folder if it does not exist
    mkdir(folder_output)
    # filename
    figure_vol_output = glue(
      "{folder_output}EnsembleVolumeHindcast_{data_input$info$catchment_code}_",
      "1st{data_input$info$month_initialisation}_{data_input$time_horizon$predictor_list_join}_",
      "{data_input$time_horizon$volume_span_text}{data_input$wy_holdout}.png")
    
    ggsave(figure_vol_output,plot = p3, width = width_p, height = height_p)
  }
  
  if (show_chart) {
    plot(p3)
    return(p3)
  }
    
}


### monthly stream flows from knn

data_plot_knn_flow <- function(data_input,q_ens_fore) {
  
  months_wy_forecast = colnames(q_ens_fore)
  months_wy_df = data.frame(wym_str = months_wy) %>% 
    mutate(wym = row_number())
  # ensemble flow forecast
  q_ens_fore = data.table(q_ens_fore) %>%
    rowid_to_column("ens")
  
  q_ens = melt.data.table(q_ens_fore,
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
  
  q_ens$wym_str <- factor(q_ens$wym_str , levels=data_input$time_horizon$months_wy)
  
  # flow observations
  q_flows = data_input$raw_data$monthly_flows[,c("wy_simple", "wym", "Q_converted")]
  
  # flow observations only water year hold.out (if exists)
  q_obs = subset(q_flows, wy_simple == data_input$wy_holdout)[,c("wym", "Q_converted")] %>%
    mutate(variable = 'wy_holdout')
  
  colnames(q_obs) = c("wym","value","variable")
  
  q_obs = q_obs %>%
    mutate(wym_str = data_input$time_horizon$months_wy[wym])
  # flow observations
  #FUN = function(x){c(mean = mean(x), len = median(x))}
  q_flow_sn_holdout = subset(q_flows, wy_simple != data_input$wy_holdout)[,c("wym", "Q_converted")]
  
  q_obs_stats = 
    aggregate(
    formula = Q_converted ~ wym,
    data = q_flow_sn_holdout,
    FUN = quantile,
    probs = c(0.05,0.10,0.25,0.5,0.75,0.9,0.95),
    drop = F
      
  ) %>% 
    data.table() %>%
    melt.data.table(id.vars = "wym") %>%
    mutate(Pexc  = 100-as.numeric(stringr::str_extract_all(variable,"(?<=Q_converted.).+(?=.)"))) %>% 
    mutate(wym_str = data_input$time_horizon$months_wy[wym])
  
  
  
  return(
    list(
      q_ens = q_ens,
      q_obs = q_obs ,
      q_obs_stats = q_obs_stats
    )
  )
}

plot_knn_flow <- function(
    data_input,
    q_ens_fore,
    export = FALSE,
    show_chart = FALSE) {
  
  library(ggplot2)
  library(see) # halfviolin
  
  plot_text = data_input$time_horizon
  
  q_plot_data = data_plot_knn_flow(data_input = data_input,q_ens_fore=q_ens_fore)
  q_plot_data$q_obs_stats$wym_str <- factor(q_plot_data$q_obs_stats$wym_str , levels=unique(q_plot_data$q_obs_stats$wym_str))
  
  median_q_ens = apply(q_ens_fore, 2, median) %>%
    t %>%
    data.table() %>%
    melt.data.table(id.vars = NULL,
                    measure.vars = all_of(colnames(q_ens_fore)) ,
                    variable.name = "wym_str",value.name = "median_flow")
  
    # observation stats
  p=ggplot()+ 
    geom_area(
      data = q_plot_data$q_obs_stats,
      aes(x=wym_str,y = value,fill = as.factor(Pexc),group = Pexc),
      alpha=0.6,
      position = position_identity()
    )+
    scale_fill_brewer(palette = "RdBu",direction = -1)+
  # add forecast
    geom_violinhalf(
      data = q_plot_data$q_ens,
      mapping =  aes(x=wym_str,y=value),
      scale = "width",
      flip = T,
      lwd = 0.1,
      width=0.5,
      alpha=0.2
    )+
    geom_boxplot(
      data = q_plot_data$q_ens,
      mapping =  aes(x=wym_str,y=value),
      #scale = "width",
      lwd = 0.1,
      width = 0.1,
      outlier.size = 0.5
    )+
    geom_point(
      data = median_q_ens,
      mapping = aes(x = wym_str, y = median_flow, color = "mediana"),
      size = 1,
      shape = 3
    )+
    geom_line(
      data = q_plot_data$q_obs,
      mapping = aes(x=wym_str,y = value, group=variable, color = variable),
      size=0.3
    )+
    geom_point(
      data = q_plot_data$q_obs,
      mapping = aes(x=wym_str,y = value, group=variable, color = variable),
      size=0.5
    )+
  # add aestetics
    scale_color_manual(
      labels = c(
      glue(" Mediana Pronóstico"),
      glue(" {data_input$wy_holdout}")
    ),
    values=c("black","green")
      
  )+
    #scale_x_discrete(expand = c(0,0))+
    theme(legend.position="bottom",
          legend.spacing.x = unit(0, 'cm'))+
    guides(
      fill = guide_legend(
      label.position = "bottom",
      nrow = 1,
      title = "Prob. Excedencia (%)",
      title.vjust = 0.8
      ),
      color=guide_legend(
        title = "Caudal ",
        title.vjust = 0.8,
        nrow=2
        )
      )+
    scale_x_discrete(expand = c(0,0))+
    labs(
     x= "",
     y = glue("Caudal ({data_input$info$units_q})")
    )
  
  #max_y = ceiling(max(q_plot_data$q_ens$value,na.rm=T)/10)*10
  ceiling_num <- function(x,num=1) {x = ceiling(x/num)*num}
  
  max_y_info = q_plot_data$q_ens[which.max(q_plot_data$q_ens$value)]
  #max_y =  max_y_info$value %>% ceiling10
  
  max_perce = merge(
    x=q_plot_data$q_obs_stats,
    y=max_y_info,
    by = c("wym","wym_str")
  ) %>% 
    mutate(x_Larger_y = value.x>value.y) %>% 
    mutate(cumsum = cumsum(x_Larger_y))
  
  position_max_perce = max(which(max_perce$cumsum>0),length(max_perce$cumsum))
  max_y_percentile = q_plot_data$q_obs_stats %>% 
    subset(Pexc == max_perce[position_max_perce]$Pexc) %$% value %>% 
    max(na.rm = T)
  
  #print(max_y_info$value,"_",max_y_percentile)
  max_y = max(max_y_info$value,max_y_percentile) %>% ceiling_num

  p1 = p +
    geom_text(
      data = median_q_ens,
      mapping = aes(x = wym_str,y = max_y*1.05,label= sprintf("%.1f",median_flow) ),
      size=3
    )+
    scale_x_discrete(
      limits=data_input$time_horizon$months_forecast_period,
      expand = c(0.05 ,0)
      )+
    scale_y_continuous(
      limits = c(NA,max_y*1.05)
    )
    

   
  #print(p)
  

  
  # idea add an inset plot with forecast period
  library(patchwork)
  
  p2 = (p)+
    plot_annotation(
         title=glue("Pronóstico del caudal medio mensual"),
         subtitle = data_input$raw_data$attributes_catchment$gauge_name,
         caption = glue("Emisión {plot_text$date_initialisation}"),
         tag_levels = "a"
    )+
    plot_layout(guides='collect') &
    theme(legend.position='bottom')

  # library(png)
  # library(RCurl)
  # library(cowplot)
  # library(magick)
  # img <- readPNG(getURLContent(url = "https://i.imgur.com/EOc2V.png"))
  # 
  # p3 = ggdraw()+
  #   draw_image(img,x = 0.3, y = 0.4, scale = .2)+
  #   draw_plot(p2)
  
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
    folder_output = glue("data_output/pronostico_caudal/Figures/ensemble_forecast/{data_input$info$catchment_code}/")
    # create folder if it does not exist
    mkdir(folder_output)
    width_p = 7.2
    height_p = 4
    #filename
    figure_q_output = glue(
      "{folder_output}flow_ensemble_forecast_{data_input$info$catchment_code}_",
      "1st{data_input$info$month_initialisation}_{plot_text$predictor_list_join}_",
      "{plot_text$volume_span_text}{data_input$wy_holdout}.png")
    
    ggsave(figure_q_output,plot = p2, width = width_p, height = height_p)
  }
   if (show_chart) {
    plot(p2)
    return(p2)
  }
  
  
}





filename_input  = filename_input  = "data_output/scores/RDS/model_results_singles_models_2023-03-15.RDS"
metric_variable = "crpss_climatology"
filename_export = "CRPSS_vol_climate_indices_pheatmap"
legend_title = "CRPSS (climatología)"
plot_absolute = F
threshold_for_x = 0.1





plot_pheatmap_EDA <- function(
    filename_input,
    metric_variable,
    legend_title,
    filename_export,
    plot_absolute,
    threshold_for_x = 0.1
) {
  #https://jokergoo.github.io/2020/05/06/translate-from-pheatmap-to-complexheatmap/
  #https://jokergoo.github.io/ComplexHeatmap-reference/book/a-list-of-heatmaps.html
  
  library(ComplexHeatmap)
  
  df <- readRDS(filename_input)
  setDT(df)
  
  # Split predictor_name into var, fun, and horizon_months
  df <- df[, c("var", "fun", "horizon_months") := tstrsplit(predictor_list, "_", fixed = TRUE)][]
  df <- df[, horizon_months := as.numeric(gsub("*months","",horizon_months))][]
  df <- df[, horizon_months := factor(horizon_months,levels = seq(-1,12,1))]
  # Create catchment_month and predictor columns
  df <- df[, catchment_month := paste(catchment_code, month_initialisation, sep = "_")][]
  df <- df[, predictor := paste(var, horizon_months, sep = "_")]
  df$predictor = factor(df$predictor,levels = unique(df$predictor))
  # Use dcast to reshape the data into a correlation matrix
  cor_matrix <-
    dcast(df,
          catchment_month ~ predictor,
          value.var = metric_variable,
          fill = NA,
          verbose = T)
  
  # Annotation for rows
  annotation_row <- data.table(catchment_month = cor_matrix[, catchment_month])
  annotation_row <-annotation_row[,
                                  c("catchment_code", "month_initialisation") := 
                                    tstrsplit(catchment_month, "_",
                                              fixed = TRUE, keep = 1:2)]
  #annotation_row$month_initialisation = lubridate::month(as.numeric(annotation_row$month_initialisation),label = T)
  annotation_row$month_initialisation = paste0('1° ',annotation_row$month_initialisation)
  
  annotation_row <- annotation_row[, catchment_code := as.numeric(catchment_code)]
  annotation_row <- annotation_row[, month_initialisation := factor(
    x = month_initialisation,
    levels = (paste0('1° ',levels(df$month_initialisation) )))]
  annotation_row <- tibble::column_to_rownames(annotation_row, var = "catchment_month")
  rows_split <-  annotation_row$month_initialisation
  annotation_row$month_initialisation <- NULL
  
  # Annotation for columns
  cor_matrix$catchment_month <- NULL
  cor_matrix <- as.matrix(cor_matrix)
  annotation_col <- data.table(predictor = colnames(cor_matrix))
  annotation_col <- annotation_col[, c("var", "horizon_months") := tstrsplit(predictor, "_", fixed = TRUE, keep = 1:2)]
  annotation_col <- annotation_col[, horizon_months := factor(horizon_months,levels = seq(-1,12,1))]
  annotation_col <- annotation_col[, var := factor(var)]
  #setkeyv(annotation_col,c("var", "horizon_months"))
  
  
  cols_split <-  annotation_col$var
  annotation_col$var <-  NULL # remove var from legend
  annotation_col$predictor <- NULL # remove predictor from legend
  colnames(cor_matrix) <- NULL #remove row labels
  
 
  
  #### colors
  length_horizon = length(unique(as.numeric(annotation_col$horizon_months)) )
  colours_blue <- colorRampPalette(c("#190E53", "#3C3176", "#887FBC"))(length_horizon)
  
  library(circlize)
  col_fun = colorRamp2(c(-1,-0.5, 0,0.5, 1), c("red","orange", "white","skyblue", "blue"))
  
  ann_colors = list(
    catchment_code = c("firebrick", 'green'),
    horizon_months = setNames(colours_blue, unique(annotation_col$horizon_months))
  )

  
  #library(RColorBrewer)
  #Plot correlation matrix and absolute correlation matrix using pheatmap
   p = 
    pheatmap((cor_matrix),
             cluster_row = F,
             cluster_cols = F,
             color = col_fun,#colorRampPalette((brewer.pal(n = 7, name = "RdBu")))(100),
             annotation_row = annotation_row,
             annotation_col = annotation_col,
             
             row_split = rows_split,
             column_split = cols_split,
             annotation_names_col = F,
             annotation_names_row = F,
             annotation_colors = ann_colors,
             heatmap_legend_param = list(
               at = c(-1, 0, 1),
               title = legend_title,
               legend_height = unit(3, "cm"),
               title_position = "topleft"
             )
             # cell_fun = function(j, i, x, y, width, height, fill) {
             #   if (cor_matrix[i, j] > -threshold_for_x &
             #       cor_matrix[i, j] < threshold_for_x)
             #     grid.text('x', x, y, gp = gpar(fontsize = 6))
             # }
    )
  #export in png format
  filename_export_png = paste0(
    "data_output/scores/figures/",
    filename_export,
    today(),
    ".png"
  )
  
  png(
    filename = filename_export_png,
    width = 10,
    height = 10,
    units = "in",
    res = 800
  )
  p_draw =
    draw(p,
       merge_legend = TRUE,
       column_title = paste0(legend_title, ": predictores climáticos vs volumen sep-mar, 1981-2021"),
       row_title = "Fecha de emisión",
       column_title_gp = gpar(fontsize = 15),
       ht_gap = unit(4, "cm"))
  
  dev.off()
  
plot_list = list(plot1 = p_draw, filename1 = filename_export_png )
  #### absolute value
  if(plot_absolute){
  new_matrix = abs(cor_matrix)
  p_abs <-
    pheatmap(
      new_matrix,
      cluster_row = F,
      cluster_cols = F,
      annotation_row = annotation_row,
      annotation_col = annotation_col,
      row_split = rows_split,
      column_split = cols_split,
      annotation_names_col = F,
      annotation_names_row = F,
      annotation_colors = ann_colors,
      heatmap_legend_param = list(
        at = c(0, 0.25, 0.5, 0.75, 1.0),
        title = legend_title,
        legend_height = unit(3, "cm"),
        title_position = "topleft"
      ),
      cell_fun = function(j, i, x, y, width, height, fill) {
        if (new_matrix[i, j] < threshold_for_x)
          grid.text('x', x, y, gp = gpar(fontsize = 4))
      }
    )
  

  #export absolute(matrix)
  filename_export_png_abs =     paste0(
    "data_output/scores/figures/",
    filename_export,
    "_abs",
    today(),
    ".png"
  )
  
  png(filename = filename_export_png_abs,
    width = 10,
    height = 10,
    units = "in",
    res = 800
  )

  p_abs_draw =
    draw(p_abs,
       merge_legend = TRUE,
       column_title = paste0(legend_title, ": predictores climáticos vs volumen sep-mar, 1981-2021"),
       row_title = "Fecha de emisión",
       column_title_gp = gpar(fontsize = 15),
       ht_gap = unit(4, "cm"))
  
  dev.off()
  plot_list = append(plot_list, list(plot2 = p_abs_draw,
                                    filename2 = filename_export_png_abs))
  }
  return(plot_list)
}






filename_input  = "data_output/scores/RDS/model_results_singles_models_2023-03-14.RDS"
metric_variable = "crpss_climatology"
filename_export = "CRPSS_vol_climate_indices_pheatmap"
legend_title = "CRPSS (climatología)"
plot_absolute = F


plot_pheatmap_EDA2 <- function(
    filename_input,
    metric_variable,
    legend_title,
    filename_export,
    plot_absolute,
    sub_variable,
    threshold_for_x = 0.1
) {
  #https://jokergoo.github.io/2020/05/06/translate-from-pheatmap-to-complexheatmap/
  #https://jokergoo.github.io/ComplexHeatmap-reference/book/a-list-of-heatmaps.html
  
  #libraries
  suppressPackageStartupMessages(library(ComplexHeatmap))
  suppressPackageStartupMessages(library(data.table)) 
  suppressPackageStartupMessages(library(circlize))
  suppressPackageStartupMessages(library(lubridate))
  
  df <- readRDS(filename_input)
  setDT(df)
  
  # Split predictor_name into var, fun, and horizon_months
  df <- df[, c("var", "fun", "horizon_months") := tstrsplit(predictor_name, "_", fixed = TRUE)][]
  df <- df[, horizon_months := as.numeric(gsub("*months","",horizon_months))][]
  df <- df[, horizon_months := factor(horizon_months,levels = seq(1,12,1))]
  # Create catchment_month and predictor columns
  df <- df[, catchment_month := paste(catchment_code, month_initialisation, sep = "_")][]
  df <- df[, predictor := paste(var,method, horizon_months, sep = "_")]
  df$predictor = factor(df$predictor,levels = unique(df$predictor))
  df = subset(df,var==sub_variable)
  # Use dcast to reshape the data into a correlation matrix
  cor_matrix <-
    dcast(df,
          catchment_month ~ predictor,
          value.var = metric_variable,
          fill = NA,
          verbose = T)
  
  # Annotation for rows
  annotation_row <- data.table(catchment_month = cor_matrix[, catchment_month])
  annotation_row <-annotation_row[,
                                  c("catchment_code", "month_initialisation") := 
                                    tstrsplit(catchment_month, "_",
                                              fixed = TRUE, keep = 1:2)]
  #annotation_row$month_initialisation = lubridate::month(as.numeric(annotation_row$month_initialisation),label = T)
  annotation_row$month_initialisation = paste0('1° ',annotation_row$month_initialisation)
  
  annotation_row <- annotation_row[, catchment_code := as.numeric(catchment_code)]
  annotation_row <- annotation_row[, month_initialisation := factor(
    x = month_initialisation,
    levels = (paste0('1° ',levels(df$month_initialisation) )))]
  annotation_row <- tibble::column_to_rownames(annotation_row, var = "catchment_month")
  rows_split <-  annotation_row$month_initialisation
  annotation_row$month_initialisation <- NULL
  
  # Annotation for columns
  cor_matrix$catchment_month <- NULL
  cor_matrix <- as.matrix(cor_matrix)
  annotation_col <- data.table(predictor = colnames(cor_matrix))
  annotation_col <- annotation_col[, c("var","method", "horizon_months") := tstrsplit(predictor, "_", fixed = TRUE, keep = 1:3)]
  annotation_col <- annotation_col[, horizon_months := factor(horizon_months,levels = seq(1,12,1))]
  annotation_col <- annotation_col[, var := factor(var)]
  #setkeyv(annotation_col,c("var", "horizon_months"))
  
  
  cols_split <-  annotation_col$method
  annotation_col$var <-  NULL # remove var from legend
  annotation_col$predictor <- NULL # remove predictor from legend
  colnames(cor_matrix) <- NULL #remove row labels
  
  
  
  #### colors
  length_horizon = length(unique(as.numeric(annotation_col$horizon_months)) )
  colours_blue <- colorRampPalette(c("#190E53", "#3C3176", "#887FBC"))(length_horizon)
  
  
  col_fun = colorRamp2(c(-1,-0.5, 0,0.5, 1), c("red","orange", "white","skyblue", "blue"))
  
  ann_colors = list(
    catchment_code = c("firebrick", 'green'),
    horizon_months = setNames(colours_blue, unique(annotation_col$horizon_months))
  )
  
  
  #library(RColorBrewer)
  #Plot correlation matrix and absolute correlation matrix using pheatmap
  p = 
    pheatmap((cor_matrix),
             cluster_row = F,
             cluster_cols = F,
             color = col_fun,#colorRampPalette((brewer.pal(n = 7, name = "RdBu")))(100),
             annotation_row = annotation_row,
             annotation_col = annotation_col,
             
             row_split = rows_split,
             column_split = cols_split,
             annotation_names_col = F,
             annotation_names_row = F,
             annotation_colors = ann_colors,
             heatmap_legend_param = list(
               at = c(-1, 0, 1),
               title = legend_title,
               legend_height = unit(3, "cm"),
               title_position = "topleft"
             ),
             cell_fun = function(j, i, x, y, width, height, fill) {
               if (cor_matrix[i, j] > -threshold_for_x &
                   cor_matrix[i, j] < threshold_for_x)
                 grid.text('x', x, y, gp = gpar(fontsize = 6))
             }
    )
  #export in png format
  filename_export_png = paste0(
    "data_output/scores/figures/",
    filename_export,
    today(),
    ".png"
  )
  
  png(
    filename = filename_export_png,
    width = 10,
    height = 10,
    units = "in",
    res = 800
  )
  p_draw =
    draw(p,
         merge_legend = TRUE,
         column_title = paste0(legend_title, ": ",sub_variable," vs volumen sep-mar, 1981-2021"),
         row_title = "Fecha de emisión",
         column_title_gp = gpar(fontsize = 15),
         ht_gap = unit(4, "cm"))
  
  dev.off()
  
  plot_list = list(plot1 = p_draw, filename1 = filename_export_png )
  
  return(plot_list)
}







filename_input  = filename_input  = "data_output/scores/RDS/model_results_singles_models_2023-03-15.RDS"
metric_variable = "crpss_climatology"
filename_export = "CRPSS_vol_climate_indices_pheatmap"
legend_title = "CRPSS (climatología)"
plot_absolute = F
sub_month = "Sep"



plot_pheatmap_EDA3 <- function(
    filename_input,
    metric_variable,
    legend_title,
    filename_export,
    plot_absolute,
    sub_variable,
    threshold_for_x = 0.1
) {
  #https://jokergoo.github.io/2020/05/06/translate-from-pheatmap-to-complexheatmap/
  #https://jokergoo.github.io/ComplexHeatmap-reference/book/a-list-of-heatmaps.html
  
  #libraries
  suppressPackageStartupMessages(library(ComplexHeatmap))
  suppressPackageStartupMessages(library(data.table)) 
  suppressPackageStartupMessages(library(circlize))
  suppressPackageStartupMessages(library(lubridate))
  
  df <- readRDS(filename_input)
  setDT(df)
  
  # Split predictor_name into var, fun, and horizon_months
  df <- df[, c("var", "fun", "horizon_months") := tstrsplit(predictor_name, "_", fixed = TRUE)][]
  df <- df[, horizon_months := as.numeric(gsub("*months","",horizon_months))][]
  df <- df[, horizon_months := factor(horizon_months,levels = seq(1,12,1))]
  
  # Create catchment_month and predictor columns
  df <- df[, catchment_month := paste(catchment_code, var, sep = "_")][]
  df <- df[, predictor := paste(method, horizon_months, sep = "_")]
  df$predictor = factor(df$predictor,levels = unique(df$predictor))
  df = subset(df,month_initialisation == sub_month)
  # Use dcast to reshape the data into a correlation matrix
  cor_matrix <-
    dcast(df,
          catchment_month ~ predictor,
          value.var = metric_variable,
          fill = NA,
          verbose = T)
  
  # Annotation for rows
  annotation_row <- data.table(catchment_month = cor_matrix[, catchment_month])
  annotation_row <-annotation_row[,
                                  c("catchment_code", "var") := 
                                    tstrsplit(catchment_month, "_",
                                              fixed = TRUE, keep = 1:2)]
  #annotation_row$month_initialisation = lubridate::month(as.numeric(annotation_row$month_initialisation),label = T)
  #annotation_row$month_initialisation = paste0('1° ',annotation_row$month_initialisation)
  
  annotation_row <- annotation_row[, catchment_code := as.numeric(catchment_code)]
  annotation_row <- tibble::column_to_rownames(annotation_row, var = "catchment_month")
  rows_split <-  annotation_row$var
  annotation_row$var <- NULL
  
  # Annotation for columns
  cor_matrix$catchment_month <- NULL
  cor_matrix <- as.matrix(cor_matrix)
  annotation_col <- data.table(predictor = colnames(cor_matrix))
  annotation_col <- annotation_col[, c("method", "horizon_months") := tstrsplit(predictor, "_", fixed = TRUE, keep = 1:2)]
  annotation_col <- annotation_col[, horizon_months := factor(horizon_months,levels = seq(1,12,1))]
  #annotation_col <- annotation_col[, var := factor(var)]
  #setkeyv(annotation_col,c("var", "horizon_months"))
  
  
  cols_split <-  annotation_col$method
  #annotation_col$var <-  NULL # remove var from legend
  annotation_col$predictor <- NULL # remove predictor from legend
  colnames(cor_matrix) <- NULL #remove row labels
  
  
  
  #### colors
  length_horizon = length(unique(as.numeric(annotation_col$horizon_months)) )
  colours_blue <- colorRampPalette(c("#190E53", "#3C3176", "#887FBC"))(length_horizon)
  
  
  col_fun = colorRamp2(c(-1,-0.5, 0,0.5, 1), c("red","orange", "white","skyblue", "blue"))
  
  ann_colors = list(
    catchment_code = c("firebrick", 'green'),
    horizon_months = setNames(colours_blue, unique(annotation_col$horizon_months))
  )
  
  
  #library(RColorBrewer)
  #Plot correlation matrix and absolute correlation matrix using pheatmap
  p = 
    pheatmap((cor_matrix),
             cluster_row = F,
             cluster_cols = F,
             color = col_fun,#colorRampPalette((brewer.pal(n = 7, name = "RdBu")))(100),
             annotation_row = annotation_row,
             annotation_col = annotation_col,
             
             row_split = rows_split,
             column_split = cols_split,
             annotation_names_col = F,
             annotation_names_row = F,
             annotation_colors = ann_colors,
             heatmap_legend_param = list(
               at = c(-1, 0, 1),
               title = legend_title,
               legend_height = unit(3, "cm"),
               title_position = "topleft"
             ),
             cell_fun = function(j, i, x, y, width, height, fill) {
               if (cor_matrix[i, j] > -threshold_for_x &
                   cor_matrix[i, j] < threshold_for_x)
                 grid.text('x', x, y, gp = gpar(fontsize = 6))
             }
    )
  #export in png format
  filename_export_png = paste0(
    "data_output/scores/figures/",
    filename_export,
    today(),
    ".png"
  )
  
  png(
    filename = filename_export_png,
    width = 10,
    height = 10,
    units = "in",
    res = 800
  )
  p_draw =
    draw(p,
         merge_legend = TRUE,
         column_title = paste0(legend_title, ": ",sub_variable," vs volumen sep-mar, 1981-2021"),
         row_title = "Fecha de emisión",
         column_title_gp = gpar(fontsize = 15),
         ht_gap = unit(4, "cm"))
  
  dev.off()
  
  plot_list = list(plot1 = p_draw, filename1 = filename_export_png )
  
  return(plot_list)
}
