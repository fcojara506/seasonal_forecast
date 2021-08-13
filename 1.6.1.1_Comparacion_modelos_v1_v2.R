# FREE ENVIRONMENT FILES
rm(list = ls())
gc()

# LOAD LIBRARIES 
load_silently <- function(lib_name) {suppressWarnings(suppressMessages(require(lib_name, character.only = TRUE)))}
sapply(c("feather","ggplot2","dplyr","data.table"),load_silently)

########## SET CURRENT DIRECTORY ###########################
setwd("~/GoogleDrive/CORFO_Maule_FJ/Pronostico_estacional")
##########FUNCTIONS#####################
#FUNCTION TO SORT X-AXIS ACCORDING TO A FUNCTION
reorderv2 <- function(x, X, FUN = mean, ..., order = is.ordered(x)){
  scores <- tapply(X = X, INDEX = x, FUN = FUN, ...)
  ans <- (if (order) ordered else factor)(x, levels = names(sort(scores, na.last =F)))
  attr(ans, "scores") <- scores
  return(ans)
}
###########INITIALISE VARIABLES ###################

wy_months_labels           <- c("abr","may","jun","jul","ago","sep","oct","nov","dic","ene","feb","mar")
propiedades_cuencas        <- read.csv("data_auxiliar/propiedades_cuencas.csv")

########## LOAD PREDICTED VOLUME ##################
read_models <- function() {

modelo_v1                  <- read_feather("data_output/pronostico_vol_estacional_v1.feather") %>%
  subset(predictando=="predictando_dinamico")

modelo_v2                  <- read_feather("data_output/pronostico_vol_estacional_v2.feather")%>%
  subset(predictando=="predictando_dinamico") %>%
  mutate(
    version=ifelse(
      version=="CI_last","Condiciones Iniciales GR6J",
      ifelse(version=="pr_sum + tem_mean","Variables Meterologicas ERA5",version))) 

modelo_v1_v2                <-rbind(modelo_v1,modelo_v2) 
return(modelo_v1_v2)
}


plot_results <- function(cuenca_target,wym_target,wy_target,version_target) {
  
  message("BASIN: ",cuenca_target,", WYM: ",wym_target,", VERSION: ",version_target)
  
  # sequence of months to predict (default 6-12: sep-mar)
  periodo_volumen           <- paste0("[",ifelse(wym_target>5,wym_target+1,6),"-12]") #"[6-12]"
  message("TARGET SEASONAL MONTHS:",periodo_volumen)
  
  # current month
  ylabel_mes                <- wy_months_labels[wym_target+1]
  
  #subset predicted volumes to basin, month and version
  predicted_vol_ensembles   <- modelo_v1_v2 %>%
    subset(cuenca  == cuenca_target) %>% 
    subset(wym     == wym_target) %>% 
    subset(version == version_target)
  
  # load volume for the specific seasonal period, basin and exclude target year
  vol.obs                   <- read_feather("data_input/MODELO_VOLUMENES_ESTACIONALES.feather") %>%
    subset(GRP    == periodo_volumen ) %>%
    subset(wy     != wy_target) %>%
    subset(cuenca == cuenca_target) %>%
    rename(wy_simple=wy) %>% 
    select(-GRP)
  
  # subset year in which both observed and predicted exist
  vol.obs.shared            <- vol.obs %>%
    subset(wy_simple %in% (predicted_vol_ensembles$wy %>% unique()))
  
  
  # compute quantile of observed volume of previous years (target year's shouldn't be here)
  cuantiles_obs             <- vol.obs$volumen %>%
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
  #EXAMPLE
  #cuantiles_obs probs_cuantiles
  #220.9768             25%
  #342.0063             50%
  #460.9763             75%
  
  # volume range between 25% and 75% quantiles
  vol_predicted_range       <- cuantiles_pred["50%","cuantiles_obs"] %>% round %>%
    paste0(" ± ") %>%
    paste0(cuantiles_pred[,"cuantiles_obs"] %>% round() %>% diff() %>% max()) %>% 
    paste0(" Mill.")
  
  text_estimacion           <- as.expression(bquote(.(vol_predicted_range) ~ m^3))
  #1-ecdf(vol.obs$volumen)(round(cuantiles_pred$cuantiles_obs)[2]) %>% print()
  
  # years colours
  years_colours                       <- predicted_vol_ensembles %$%
    reorder(wy,volumen,FUN = median)  %>%
    levels() %>%
    {(.==wy_target)} %>%
    ifelse("red","black")
  
  
  ggplot()+
    geom_boxplot(data=predicted_vol_ensembles,
                 aes(x=reorderv2(wy,volumen,FUN = median),y=volumen,fill=version),
                 fill="grey80",
                 width=0.7,
                 color="black",
                 outlier.shape = NA,
                 lwd=0.1)+
    
    #scale_fill_brewer(palette = "Dark1")
    theme_bw()+
    theme(legend.position="bottom",
          axis.text.x=element_text(angle=90,hjust=1,colour = years_colours),
          legend.background = element_blank())+
    scale_x_discrete(expand = expansion(mult = c(0.08, 0.02)))+
    geom_point(data=vol.obs.shared,aes(x=wy_simple,y=volumen,col="Caudales medidos"),size=2)+
    scale_color_manual(values = c('Caudales medidos' = 'red'),name = "")+
    geom_abline(data=cuantiles_obs,aes(slope=0,intercept=cuantiles_obs),linetype="dashed")+
    geom_text(data=cuantiles_obs,aes(x=0.25,y=cuantiles_obs,label=probs_cuantiles),vjust = 0,hjust = 0.5)+
    facet_wrap(~version)+
    ggtitle(paste0("Pronóstico del ", subset(propiedades_cuencas,nombre_corto==cuenca_target)$nombre_completo,". Inicializado el 1° de ",wy_months_labels[wym_target+1]," ",wy_target))+
    labs( x="Año hidrológico (ordenado por mediana del pronóstico)",
          y= as.expression(bquote("Volumen"~.(ylabel_mes)~ "-mar (Millones"~m^3~")")),
          col="Caudales medidos",
          fill=""
    )+
    ylim(0,NA)+
    annotate(geom = 'text', x=5, y = Inf, hjust = 1, vjust = 1,label=text_estimacion)
  
  
  
  figures_filename=paste0("Figuras/test/",wy_months_labels[wym_target],"/comparacion_vol_pronosticado_",
                          cuenca_target,
                          "_wym:",wym_target,
                          "_version:",version_target,
                          ".png") %>%
    ggsave(width = 9, height = 3.5) 
  
  data_export=data.frame(
    id=1,
    cuenca= subset(propiedades_cuencas,nombre_corto==cuenca_target)$codigo_dga,
    vol_normal=cuantiles_obs$cuantiles_obs[2],
    vol_pasado=subset(vol.obs,wy_simple==wy_target-1)$volumen,
    vol_promax=max(cuantiles_pred$cuantiles_obs),
    vol_promin=min(cuantiles_pred$cuantiles_obs),
    vol_pron=cuantiles_pred$cuantiles_obs[2]) %>%
    mutate( difporc_2018=(vol_pron-vol_pasado)/vol_pasado,
            difporc_promedio=(vol_pron-vol_normal)/vol_normal,
            year=wy_target,
            cuenca_name=cuenca_target,
            mes=wy_months_labels[wym_target+1],
            metodo=version_target
    )
  return(data_export)
}

######## USER OPTIONS #############################

probs_cuantiles             <- c(0.05,0.5,0.95)
target_basins               <- c("Maule","Achibueno","Lontue","Melado","Longavi","Ancoa")
wy_target                   <- 2021
wym_target                  <- 4  #1=apr,2=may,3=jun,4=jul,5=ago,6=sep,7=oct,8=nov,9=dic,10=ene,11=feb,12=mar
#cuenca_target               <- target_basins[1]

####### auxiliar variables #######################
results                     <- list()
w                           <- 1
modelo_v1_v2                <- read_models()

for (cuenca_target in target_basins) {
  
  versiones <- modelo_v1_v2 %>% 
    subset(cuenca==cuenca_target) %>% 
    subset(wym==wym_target) %$%
    version %>% 
    unique()
  
  for (version_target in versiones) {
    results[[w]]=plot_results(cuenca_target = cuenca_target,
                              wym_target = wym_target,
                              wy_target = wy_target,
                              version_target = version_target)
    w=w+1
  }
}

results=results %>%
  rbindlist %>%
  write.csv(paste0("Exportar_plataforma/volumen_temporada_",today(),".csv"),row.names=FALSE)
