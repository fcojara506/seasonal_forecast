# FREE ENVIRONMENT FILES
rm(list = ls())
gc()

# LOAD LIBRARIES 
load_silently <- function(lib_name) {suppressWarnings(suppressMessages(require(lib_name, character.only = TRUE)))}
sapply(c("feather","ggplot2","dplyr","data.table","lubridate"),load_silently)

########## SET CURRENT DIRECTORY ###########################
setwd("~/GoogleDrive/CORFO_Maule_FJ/Pronostico_estacional")

#INPUT VARIABLES
mes_inicializacion              <- "oct"
wy_target                       <- 2021

r1 <- function(variable) {round(variable,1)}

read_streamflow_forecasts <- function(version,cuenca_name) {
  # STREAMFLOW'S ENSEMBLE FORECASTS
  frcst_streamflows                <- 
    feather::read_feather(paste0("data_output/caudales_pronosticados_",version,".feather")) %>% 
    subset(cuenca == cuenca_name) %>%
    subset(version == version)
  return(frcst_streamflows)
}

organised_input_data <- function(version, cuenca_name, version_predictors, frcst_streamflows,regression_target) {

  months_wy                      <- c("abr","may","jun","jul","ago","sep","oct","nov","dic","ene","feb","mar")
  
  seasonal_months_num           <- ifelse(which(tolower(mes_inicializacion)== months_wy)>5,
                                          which(tolower(mes_inicializacion)== months_wy),
                                          6) %>% seq(12) # wy
  
  # MONTHLY STATS ABOUT THE FORECASTED STREAMFLOWS 
  stats_frcst_streamflows       <- 
    frcst_streamflows %>%
    subset(predictores == version_predictors) %>%
    subset(regression_type == regression_target) %>% 
    .[,(paste0("Q_",months_wy[seasonal_months_num]))] %>% 
    reshape2::melt(id.vars=NULL) %>%
    rename(Q_mes=variable,caudal=value) %>%
    data.table() %>% 
    .[,list(
      lower   = quantile(caudal, .25, na.rm=TRUE)%>% r1,
      middle  = quantile(caudal, .50, na.rm=TRUE)%>% r1,
      upper   = quantile(caudal, .75, na.rm=TRUE)%>% r1,
      mean    = mean(caudal,na.rm = T)%>% r1), by = 'Q_mes'] %>%
    mutate(dif   = pmax(upper-middle,middle-lower)) %>% 
    mutate(text  = paste0(round(middle,1)," ± ",round(dif,1)))
  
  # MONTHLY GAUGED STREAMFLOWS
  # VERSION 1: STATION BASED
  #if(version=="v1"){
    # gauged_streamflows             <- 
    #   paste0("data_output/caudales_mensuales_",cuenca_name,".csv") %>%
    #   read.csv(header = T,sep=",",stringsAsFactors = F ) %>%
    #   subset(select= c("WY",paste0("Q_",months_wy))) %>%
    #   reshape2::melt(id.vars="WY") %>%
    #   rename(Q_mes=variable,caudal=value) %>%
    #   data.table()
    # 
   # periodo_promedio            <- ifelse(cuenca_name!="Ancoa", "Promedio 2001-2019", "Promedio 2008-2019")
  #}
  
  # VERSION 2: HYDROLOGICAL MODEL+ GRIDDED DATA BASED
  #if(version=="v2"){
    
    gauged_streamflows            <- 
      read_feather("data_output/caudal_medio_mensual_1988_presente.feather")%>%
      subset(cuenca %in% cuenca_name) %>%
      select(-cuenca) %>% 
      `colnames<-`(c("WY",paste0("Q_",months_wy)))%>%
      reshape2::melt(id.vars="WY") %>%
      rename(Q_mes=variable,caudal=value) %>%
      data.table()
    
    periodo_promedio             <-"Promedio 1988-2020"
    
    gauged_streamflows[WY==wy_target & Q_mes %in% paste0("Q_",months_wy[seasonal_months_num])]$caudal=NA
  #}
  
  # MONTHLY STATS ABOUT THE GAUGED STREAMFLOWS 
  stats_gauged_streamflows       <- 
    gauged_streamflows[WY<wy_target,list(
      lower   = quantile(caudal, .25, na.rm=TRUE),
      middle  = quantile(caudal, .50, na.rm=TRUE),
      upper   = quantile(caudal, .75, na.rm=TRUE),
      mean    = mean(caudal,na.rm = T)),
      by='Q_mes']
  
  # SET OF SELECTED YEARS TO PLOT unique(subset(frcst_streamflows,predictores==version_predictors,select=ranking))
  WY_selection                  <- c(1996,
                                     wy_target-1,
                                     wy_target,
                                     2016)
  special_years_streamflows     <- subset(gauged_streamflows,WY %in% WY_selection)
  
  
  streamflow_previous_frcst     <-
    paste0("data_auxiliar/pronosticos_mensuales_CORFO.csv")%>%
    read.csv() %>%
    subset(cuenca == cuenca_name) %>%
    subset(wy     == wy_target) %>%
    mutate(Q_mes  = paste0("Q_",tolower(Q_mes))) %>% 
    mutate(labels              = "Pronóstico meses previos")
  
  title_cuenca                  <- 
    read.csv("data_auxiliar/propiedades_cuencas.csv") %>%
    subset(nombre_corto==cuenca_name) %>%
    select(nombre_completo) %>% 
    unlist
  
  input_streamflows                   <- list(
    stats_gauged_streamflows  = stats_gauged_streamflows,
    stats_frcst_streamflows   = stats_frcst_streamflows,
    special_years_streamflows = special_years_streamflows,
    streamflow_previous_frcst = streamflow_previous_frcst,
    gauged_streamflows        = gauged_streamflows,
    months_wy                 = months_wy,
    title_cuenca              = title_cuenca,
    version_predictors        = version_predictors,
    cuenca_name               = cuenca_name,
    mes_inicializacion        = mes_inicializacion,
    version                   = version,
    version_predictors        = version_predictors,
    periodo_promedio          = periodo_promedio,
    regression_type           = regression_target)
  
  return(input_streamflows)
  
}  



plot_monthly_forecast <- function(input_streamflows) {
  
  # stats_gauged_streamflows, stats_frcst_streamflows, special_years_streamflows, months_wy,title_cuenca,version_predictors 
  p=ggplot()+
    geom_line(data       = input_streamflows$stats_gauged_streamflows,     aes(x=Q_mes,y=mean,group=1,col=input_streamflows$periodo_promedio),size=1.2)+
    geom_ribbon(data     = input_streamflows$stats_frcst_streamflows,      aes(ymin=lower,ymax=upper,x=Q_mes,fill = "Rango P.exc 75%-25% ",group=1))+
    geom_line(data       = input_streamflows$stats_frcst_streamflows ,     aes(x=Q_mes,y=middle,group=1,col="Mediana Pronóstico"),size=1.2,linetype=2)+
    geom_line(data       = input_streamflows$special_years_streamflows,    aes(x=Q_mes,y=caudal,col=as.character(WY),group=WY),size=1.2)+
    scale_x_discrete(labels=input_streamflows$months_wy,expand = c(0,0))+
    scale_fill_manual("",  values=c("grey75","blue"))+
    scale_color_manual("", values = c("#4E873D","orange","#C82027","blue","black","grey50"))+
    labs(x="Mes del año hidrológico",
         y=expression(paste("Caudal medio mensual (",m^3,"/s)",sep="")),
         col="Caudal medidos",
         fill="",
         title=paste0("Pronóstico de caudal medio mensual"),
         subtitle=paste0("Cuenca: ",input_streamflows$title_cuenca),
         pch="",
         caption = paste0("Predictores: ", input_streamflows$version_predictors,"\n *Los caudales medidos del año 2021 son referenciales"))+
    #theme_gray()+
    theme(legend.position="bottom",
          legend.spacing.x = unit(1.0, 'mm'),
          plot.caption = element_text(hjust = 0))+
    guides(col=guide_legend(nrow=3,byrow=TRUE),
           fill=guide_legend(nrow=3,byrow=TRUE))+
    geom_pointrange(data = input_streamflows$streamflow_previous_frcst, aes(x=Q_mes,y=mediana,ymin=min,ymax=max,pch=labels))

 
  # figure's name
  directory_figures=paste0("Figuras/pronostico_caudales/1_",toupper(input_streamflows$mes_inicializacion),"_",wy_target)
  directory_figures2=paste0(directory_figures,"/",input_streamflows$cuenca_name)
  
  dir.create(directory_figures,showWarnings = FALSE)
  dir.create(directory_figures2,showWarnings = FALSE)
  
  filename=paste0(directory_figures2,"/",
         input_streamflows$cuenca_name,
         input_streamflows$mes_inicializacion,
         input_streamflows$version,
         gsub(" / ",".",input_streamflows$version_predictors),
         input_streamflows$regression_type,
         ".png")
  
  message(filename)
  
  ggsave(filename, width = 7.5, height = 4, plot = p)
  return(p)
}


export_platform <- function(input_streamflows) {
  
  q_current_year       = input_streamflows$gauged_streamflows %>% subset(WY==wy_target) %>% data.table(key="Q_mes") %>% mutate(caudal=r1(caudal))
  q_last_year          = input_streamflows$gauged_streamflows %>% subset(WY==wy_target-1) %>% select(caudal)
  q_normal             = subset(input_streamflows$stats_gauged_streamflows) %>% select(mean)
  propiedades_cuencas  = read.csv("data_auxiliar/propiedades_cuencas.csv")
  cuenca_name          = input_streamflows$cuenca_name
  months_wy_eng        <- c("apr","may","jun","jul","aug","sep","oct","nov","dec","jan","feb","mar")
  
  exportar_plataforma=
    input_streamflows$stats_frcst_streamflows[,c("Q_mes","middle","lower","upper")] %>%
    data.table(key="Q_mes") %>%
    .[q_current_year]%>%
    mutate(cuenca_id    = subset(propiedades_cuencas,nombre_corto==cuenca_name)$cuenca_id) %>% 
    mutate(cuenca       = subset(propiedades_cuencas,nombre_corto==cuenca_name)$codigo_dga %>% paste0("0",.)) %>% 
    mutate(mes          = match(months_wy_eng,tolower(month.abb))[which(input_streamflows$months_wy %in% stringr::str_remove(Q_mes,"Q_"))]) %>% 
    mutate(fecha        = paste0("01-",mes,"-",ifelse(mes>3,wy_target,wy_target+1)) %>% as.Date(tryFormats = "%d-%m-%Y") %>% format("%d-%m-%Y")) %>% 
    mutate(mes          = NULL) %>% 
    mutate(cuenca_name  = cuenca_name) %>% 
    mutate(q_last_year  = q_last_year$caudal %>% r1) %>% 
    mutate(q_normal     = q_normal$mean%>% r1) %>% 
    mutate(version      = input_streamflows$version) %>% 
    mutate(predictores  = input_streamflows$version_predictors) %>%
    rename(q_pron       = middle,
           q_pron_min   = lower,
           q_pron_max   = upper,
           q_obs        = caudal)
  
  return(exportar_plataforma[,c("cuenca_id","cuenca","cuenca_name","fecha","q_pron","q_pron_min","q_pron_max","q_last_year","q_normal","q_obs","version","predictores")])
}


#AUXILIAR VARIABLES
exportar_plataforma             <- list()
w                               <- 1

for (cuenca_name in c("Melado","Achibueno", "Maule","Lontue","Longavi","Ancoa")) {
  for (vers in c("v2")) {
    
  frcst_streamflows=read_streamflow_forecasts(vers,cuenca_name = cuenca_name)
  
  for (regression_target in unique(frcst_streamflows$regression_type)) {
    
  
  for (version_predictors in unique(frcst_streamflows$predictores)) {
    input_streamflows=
      organised_input_data(version            = vers,
                           cuenca_name        = cuenca_name,
                           version_predictors = version_predictors,
                           frcst_streamflows  = frcst_streamflows,
                           regression_target  = regression_target)
    
    plot_monthly_forecast(input_streamflows = input_streamflows)
    exportar_plataforma[[w]]=export_platform(input_streamflows = input_streamflows)
    w=w+1
  }
}
  }
}

tabla=
exportar_plataforma %>%
  rbindlist() %>%
  data.table(key="cuenca_id")

  write.csv(tabla,paste0("data_output/plataforma/pronostico_caudal",today(),".csv"),row.names=FALSE)

export_tabla_seminarios=tabla %>%
  subset(version=="v2") %>% 
  subset(predictores=="SP / PROD / ROUT") %>% 
  select(cuenca_name,fecha,q_pron,q_pron_min,q_pron_max,predictores) %>% 
  na.omit() %>% 
  t
write.csv(export_tabla_seminarios,paste0("data_output/plataforma/caudales_pronosticados_seminario",today(),".csv"),row.names=T)
