# FREE ENVIRONMENT FILES
rm(list = ls())
gc()

# LOAD LIBRARIES 
load_silently <- function(lib_name) {suppressWarnings(suppressMessages(require(lib_name, character.only = TRUE)))}
sapply(c("feather","ggplot2","dplyr","data.table"),load_silently)

########## SET CURRENT DIRECTORY ###########################
setwd("~/GoogleDrive/CORFO_Maule_FJ/Pronostico_estacional")
##########FUNCTIONS#####################

########## LOAD PREDICTED VOLUME ##################
read_models <- function(tipo_predictando="predictando_dinamico") {
  
  #modelo_v1               <-
  #  read_feather("data_output/pronostico_vol_estacional_v1.feather") %>%
  #  subset(predictando==tipo_predictando)
  
  modelo_v2               <-
    read_feather("data_output/pronostico_vol_estacional_v2.feather")%>%
    subset(predictando==tipo_predictando)
  
  #vol_ensemble_predicted   <-modelo_v2#rbind(modelo_v1,modelo_v2) 
  
  return(modelo_v2)
}


organised_input_volume <- function(cuenca_target,
                                   wym_target,
                                   wy_target,
                                   version_target,
                                   vol_ensemble_predicted,
                                   vol_obs,
                                   regression_target,
                                   tipo_predictando) {
  
  # SEQUENCE OF MONTHS TO PREDICT (DEFAULT 6-12: SEP-MAR, DYNAMIC AFTER SEPTEMBER)
  periodo_volumen       <- ifelse(tipo_predictando=="predictando_estatico",
                                  "[6-12]", #FIJO SEP-MAR
                                  paste0("[",ifelse(wym_target>5,wym_target+1,6),"-12]")) # MOVIL ENTRE SEP-MAR
  message("BASIN: ",cuenca_target,", WYM: ",wym_target,", VERSION: ",version_target, ", Regr:",regression_target, ", periodo:", periodo_volumen)
  # current month
  ylabel_mes                <- wy_months_labels[wym_target+1]
  
  #subset predicted volumes to basin, month and version
  predicted_vol_ensembles   <- vol_ensemble_predicted %>%
    subset(cuenca  == cuenca_target) %>% 
    subset(wym     == wym_target) %>% 
    subset(version == version_target) %>% 
    subset(regression_type %in% regression_target)
  
  # load volume for the specific seasonal period, basin and exclude target year
  vol.obs                   <- vol_obs %>%
    subset(cuenca == cuenca_target) %>%
    subset(GRP    == periodo_volumen ) %>%
    subset(wy     != wy_target) %>%
    rename(wy_simple = wy) %>% 
    select(-GRP)
  
  # subset year in which both observed and predicted exist
  vol.obs.shared            <- vol.obs %>%
    subset(wy_simple %in% (predicted_vol_ensembles$wy %>% unique()))
  
  # verification scores
  #scores                    <- verification_scores(predicted_vol_ensembles,vol.obs.shared)
  scores                    <- list_scores %>%
    subset(version==version_target) %>% 
    subset(wym == wym_target) %>% 
    subset(cuenca == cuenca_target) %>% 
    subset(periodo == periodo_volumen) %>% 
    subset(predictando == tipo_predictando) %>% 
    subset(regression_type == regression_target)

  # compute quantile of observed volume of previous years (target year's shouldn't be here)
  probs_cuantiles           <- c(0.05,0.5,0.95)
  
  cuantiles_obs             <- vol.obs.shared$volumen %>%
    quantile(probs = probs_cuantiles) %>%
    data.frame() %>%
    mutate(probs_cuantiles = paste0(probs_cuantiles*100,"%")) %>%
    rename(cuantiles_obs = ".")
  #EXAMPLE
  #cuantiles_obs probs_cuantiles
  #230.3275              5%
  #513.5428             50%
  #910.7378             95%
  
  # compute quantile of predicted volume of the predicted year  
  cuantiles_pred            <- predicted_vol_ensembles %>% 
    subset(wy==wy_target) %$%
    volumen %>%
    quantile(probs=c(0.25,0.5,0.75)) %>%
    data.frame() %>%
    mutate(probs_cuantiles=paste0(c(0.25,0.5,0.75)*100,"%")) %>%
    rename(cuantiles_obs=".")
  
  # volume range between 25% and 75% quantiles
  vol_predicted_range       <- cuantiles_pred["50%","cuantiles_obs"] %>% round %>%
    paste0(" ± ") %>%
    paste0(cuantiles_pred[,"cuantiles_obs"] %>% round() %>% diff() %>% max()) #%>% 
    #paste0(" Mill.")
  
  text_estimacion           <- vol_predicted_range#as.expression(bquote(.(vol_predicted_range) ~ m^3))
  
  # years colours
  years_colours                       <- predicted_vol_ensembles %$%
    reorder(wy,volumen,FUN = median)  %>%
    levels() %>%
    {(.==wy_target)} %>%
    ifelse("red","black")
  
  # figure's name
  directory_figures=paste0("Figuras/pronostico_volumenes/1_",toupper(wy_months_labels[wym_target+1]),"_",wy_target)
  directory_figures2=paste0(directory_figures,"/",cuenca_target)
  
  
  dir.create(directory_figures,showWarnings = FALSE)
  dir.create(directory_figures2,showWarnings = FALSE)
 
  figure_filename=paste0(directory_figures2,"/comparacion_vol_pronosticado_",
                         cuenca_target,
                         "_wym:",wym_target,
                         "_version:",gsub(" / ",".",version_target),
                         "_regression_type:",regression_target,
                         ".png")
  
  
  input_volume = list(predicted_vol_ensembles  = predicted_vol_ensembles,
                      vol.obs.shared           = vol.obs.shared,
                      cuantiles_obs            = cuantiles_obs,
                      scores                   = scores,
                      text_estimacion          = text_estimacion,
                      figure_filename          = figure_filename,
                      years_colours            = years_colours,
                      ylabel_mes               = ylabel_mes,
                      regression_type          = regression_target)
  
  return(input_volume)
}

#FUNCTION TO SORT X-AXIS ACCORDING TO A FUNCTION
reorderv2 <- function(x, X, FUN = mean, ..., order = is.ordered(x)){
  scores <- tapply(X = X, INDEX = x, FUN = FUN)
  ans <- (if (order) ordered else factor)(x, levels = names(sort(scores)))
  attr(ans, "scores") <- scores
  return(ans)
}

plot_timeseries_figure <- function(input_volume) {
  # predicted_vol_ensemble, vol.obs.shared, cuantiles_obs, scores$crpss, text_estimacion, figure_filename, years_colours
  #PLOT
  
  
  x_labels=input_volume$predicted_vol_ensembles$wy %>% as.character()
  
  years_colours                       <- x_labels  %>% {(.==wy_target)} %>% ifelse("red","black")
  
  p=ggplot()+
    geom_boxplot(data=input_volume$predicted_vol_ensembles,
                 aes(x=x_labels, y=volumen, fill=version),
                 fill="darkslategray2",
                 width=0.7,
                 color="black",
                 outlier.shape = NA,
                 lwd=0.1)+
    geom_point(data=input_volume$vol.obs.shared,aes(x=wy_simple,y=volumen,col="Caudales medidos",group=cuenca),size=1.5)+
    geom_line(data=input_volume$vol.obs.shared,aes(x=wy_simple,y=volumen,col="Caudales medidos",group=cuenca),size=0.5)+
    scale_x_discrete(expand = expansion(mult = c(0.08, 0.02)))+
    scale_color_manual(values = c('Caudales medidos' = 'red'),name = "")+
    geom_abline(data=input_volume$cuantiles_obs,aes(slope=0,intercept=cuantiles_obs),linetype="dashed")+
    geom_text(data=input_volume$cuantiles_obs,aes(x=0.25,y=cuantiles_obs,label=probs_cuantiles),vjust = 0,hjust = 0.5)+
    #facet_wrap(~version)+
    ggtitle(paste0("Pronóstico del ", subset(propiedades_cuencas,nombre_corto==cuenca_target)$nombre_completo,". Inicializado el 1° de ",wy_months_labels[wym_target+1]," ",wy_target))+
    labs( x="Año hidrológico",
          #subtitle = input_volume$regression_type,
          #y= as.expression(bquote("Volumen"~.(input_volume$ylabel_mes)~ "-mar (Millones"~m^3~")")),
          subtitle=as.expression(bquote("Volumen"~.(toupper(input_volume$ylabel_mes))~ "- MAR"~". Año 2021="~.(input_volume$text_estimacion)~"Millones"~m^3)),
          #col="Caudales medidos",
          col="",
          fill="",
          y=""
          #caption = paste0("CRPSS (c/r promedio) = ",input_volume$scores$crpss,"\n Regresión = ",input_volume$regression_type)
    )+
    theme(#legend.position="bottom",
          axis.text.x=element_text(angle=90,hjust=1,colour = years_colours),
          legend.background = element_blank(),
          legend.position="none")+
    ylim(0,NA)
    
  
  
  #print(p)
  #ggsave(input_volume$figure_filename %>% sub("comparacion_vol_pronosticado_","serie_tiempo_pronostico_",.),
  #       plot=p,width = 9, height = 3.5)
  
  
  return(p)
}


plot_figure <- function(input_volume) {
  # predicted_vol_ensemble, vol.obs.shared, cuantiles_obs, scores$crpss, text_estimacion, figure_filename, years_colours
  #PLOT
  
  x_labels= reorderv2(x=input_volume$predicted_vol_ensembles$wy,
              X=input_volume$predicted_vol_ensembles$volumen,
              FUN = median)
  
  years_colours                       <- x_labels  %>%
    levels() %>%
    {(.==wy_target)} %>%
    ifelse("red","black")
  
  p <- ggplot()+
    geom_boxplot(data=input_volume$predicted_vol_ensembles,
                 aes(x=x_labels, y=volumen, fill=version),
                 fill="grey80",
                 width=0.7,
                 color="black",
                 outlier.shape = NA,
                 lwd=0.1)+
    theme_bw()+
    scale_x_discrete(expand = expansion(mult = c(0.08, 0.02)))+
    geom_point(data=input_volume$vol.obs.shared,aes(x=wy_simple,y=volumen,col="Caudales medidos"),size=1.5)+
    scale_color_manual(values = c('Caudales medidos' = 'red'),name = "")+
    geom_abline(data=input_volume$cuantiles_obs,aes(slope=0,intercept=cuantiles_obs),linetype="dashed")+
    geom_text(data=input_volume$cuantiles_obs,aes(x=0.25,y=cuantiles_obs,label=probs_cuantiles),vjust = 0,hjust = 0.5)+
    #facet_wrap(~version)+
    #ggtitle(paste0("Pronóstico del ", subset(propiedades_cuencas,nombre_corto==cuenca_target)$nombre_completo,". Inicializado el 1° de ",wy_months_labels[wym_target+1]," ",wy_target))+
    labs( x="Año hidrológico (ordenado por la mediana del pronóstico)",
          #subtitle = input_volume$regression_type,
          #y= as.expression(bquote("Volumen"~.(input_volume$ylabel_mes)~ "-mar (Millones"~m^3~")")),
          col="Caudales medidos",
          fill="",
          y="",
          caption = paste0("CRPSS (c/r promedio)        = ",input_volume$scores$crpss,
                           "\nRegresión                            = ",input_volume$regression_type,
                           "\nPredictores                          = ", unique(input_volume$predicted_vol_ensembles$version))
          #caption = paste0("CRPSS=1-CRPS_ensemble/CRPS_Vol_promedio= ",input_volume$scores$crpss)
    )+
    ylim(0,NA)+
    theme(legend.position="bottom",
          axis.text.x=element_text(angle=90,hjust=1,colour = years_colours),
          legend.background = element_blank(),
          plot.caption = element_text(hjust = 0))
    #annotate(geom = 'text', x=7, y = Inf, hjust = 1, vjust = 1,label=input_volume$text_estimacion)
  
  #print(p)
  #ggsave(input_volume$figure_filename, plot=p,width = 9, height = 3.5)
  #ggsave(input_volume$figure_filename, plot=p,width = 9, height = 3)
  return(p)
}

######## USER OPTIONS #############################

target_basins               <- c("Maule","Achibueno","Lontue","Melado","Longavi","Ancoa")
wy_target                   <- 2021
#wym_target                  <- 4  
list_scores                 <- read_feather("data_output/list_scores_seasonal_volume.feather")
tipo_predictando            <- "predictando_dinamico"
propiedades_cuencas         <- read.csv("data_auxiliar/propiedades_cuencas.csv")
vol_ensemble_predicted      <- read_models(tipo_predictando)
vol_obs                     <- read_feather("data_input/MODELO_VOLUMENES_ESTACIONALES.feather")
wy_months_labels            <- c("abr","may","jun","jul","ago","sep","oct","nov","dic","ene","feb","mar")
####### auxiliar variables #######################

w                           <- 1

for (wym_target in 6) {#1=apr,2=may,3=jun,4=jul,5=ago,6=sep,7=oct,8=nov,9=dic,10=ene,11=feb,12=mar seq(1,11)
  
  for (cuenca_target in target_basins) {
    
    version_and_regre <- vol_ensemble_predicted %>% 
      subset(cuenca   == cuenca_target) %>% 
      subset(wym      == wym_target) %>% 
      select(version,regression_type) %>% 
      unique()
    
    for (regression_target in unique(version_and_regre$regression_type)) {
      
      versiones= version_and_regre %>%
        subset(regression_type==regression_target) %>% 
        select(version)
      
      for (version_target in versiones$version) {
        
        input_volume  <- organised_input_volume(cuenca_target,
                                                wym_target,
                                                wy_target,
                                                version_target,
                                                vol_ensemble_predicted,
                                                vol_obs,
                                                regression_target,
                                                tipo_predictando = tipo_predictando)
        #stop()
        p1=plot_timeseries_figure(input_volume = input_volume)
        p2=plot_figure(input_volume = input_volume)
        p3=gridExtra::arrangeGrob(p1, p2)
        ggsave(input_volume$figure_filename,plot = p3,width = 9*0.8, height = 6)
        
        w=w+1
      }
    }
  }
}


#source("1.7_plotear_list_scores.R")
#source("1.7_knn_caudales_mensuales_v2.R")
