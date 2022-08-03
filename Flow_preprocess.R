####
rm(list = ls())
library(dplyr)
library(data.table)


directory = "~/CAPTA/Pronostico_estacional/CAMELS_preproceso/"
directory_export = "~/CAPTA/Pronostico_estacional/"

setwd(directory)
#return relevant dates related to CAMELS-CL's availability
relevant_dates <- function(t_ini.chr,t_fin.chr) {
  t_ini     <- t_ini.chr %>% as.Date(format = "%Y-%m-%d")
  t_fin     <- t_fin.chr %>% as.Date(format = "%Y-%m-%d")
  
  t_seq      <- seq(from = t_ini, to = t_fin, by = "day")
  t_seq.chr  <- t_seq %>% as.character
  
  #t_d  <- t_seq %>% format("%d")
  t_m  <- t_seq %>% format("%m") #%>% factor(levels = unique(.))
  t_Y  <- t_seq %>% format("%Y")
  t_wy <- t_m %in% c("01", "02", "03") %>% ifelse(t_Y %>% as.numeric %>% `-`(1) %>% as.character, t_Y)
  agnos <- unique(t_wy)
  #meses <- unique(t_m)
  
  dates = list(t_seq.chr = t_seq.chr,t_m = t_m, t_wy = t_wy,agnos = agnos)
  return(dates)
}
## funcion cobertura de datos Q
fracCob <- function(x){sum(!is.na(x)) / length(x)}

## fill flows

fill_data_daily_pca_to_monthly<- function(Data_Q,limits_dates,codes_catchments,umbral_dias = 0.8) {
  library(missMDA)
  # meses sobre 80% data se consideran completos,
  #meses bajo 80% se declaran faltantes y se rellenan
  #umbral_dias  <- 0.80
  
  ## datos a mensuales
  Data_Q.m     <- Data_Q %>%
    as.data.frame %>%
    cbind(MES = limits_dates$t_m, WY = limits_dates$t_wy, .) %>% 
    reshape2::melt(id = c("MES", "WY")) %>%
    setNames(c("MES", "WY", "codes_catchments", "Q"))
  
  # cobertura de datos
  Data_cob_mes <- Data_Q.m %>%
    group_by(WY, MES, codes_catchments) %>%
    summarise(COB = fracCob(Q))
  
  Data_Q_mes   <- Data_Q.m %>%
    group_by(WY, MES, codes_catchments) %>%
    summarise(QM = mean(Q, na.rm = T), N = length(Q), Q = QM * N) %>%
    `[`( , c("WY", "MES", "codes_catchments", "Q"))
  
  Data_Q_mes$Q[Data_cob_mes$COB < umbral_dias] <- NA
  
  ## da formato para rellenar
  Data_Q_lista = list()
  for (imonth in c(4:12,1:3)) {
    
    char_month = sprintf("%02d",imonth) %>%  as.character()
    Q_data_month = subset(Data_Q_mes, MES == char_month) %>%
      reshape2::acast(WY ~ codes_catchments, value.var = "Q") 
    
    ## ojo: solo 2 pc para feb/mar, todo el resto 1 pc
    #cp_month =   ifelse(imonth %in% c(2,3),2,1)
    
    Q_data_month_rellenada = Q_data_month %>%
      imputePCA(ncp = 1, scale = F, maxiter = 10000) %$% completeObs %>%
      data.table() %>% 
      mutate(MES = char_month) %>% 
      cbind(WY = row.names(Q_data_month))
    
    Data_Q_lista[[char_month]] <- Q_data_month_rellenada
  }
  
  Data_Q_mes.f                   <- Data_Q_lista %>%
    rbindlist()
  
  Data_Q_mes.f[Data_Q_mes.f < 0] <- 0
  
  Data_Q_mes.f     <- Data_Q_mes.f %>%
    reshape2::melt(id.vars= c("WY","MES"),
                   variable.name = "codes_catchments",
                   value.name = "Q", as.is = T)
  
  return(Data_Q_mes.f)
}

# month to water year month
month_to_wym<- function(month_as_char)  {
  wym = as.numeric(as.character(month_as_char))
  wym = ifelse(wym>3,wym-3,wym+12-3)
  wym = sprintf("%02d", wym)
  return(wym)
}

## cuencas regimen natural seleccionadas
codes_catchments = as.character(subset(read.csv("Cuencas_Fondef-DGA_v1.csv",sep=";"),Considerar==1)$gauge_id)

## atributos cuencas
Data_attr        <- fread(file = "CAMELS_CL_v202201/catchment_attributes.csv",
                          sep = ",",
                          header = T,
                          check.names = F,
                          stringsAsFactors = F) %>%
  mutate(gauge_id = as.character(gauge_id)) %>% 
  subset(gauge_id %in% codes_catchments) %>%
  rename(cod_cuenca = gauge_id) %>% 
  .[,1:16]
  
# catchment names
limits_dates = relevant_dates(t_ini.chr="1981-04-01",t_fin.chr="2022-05-31")

## data Q diaria para el periodo seleccionado y cuencas elegidas
Data_Q <- "CAMELS_CL_v202201/q_mm_day.csv" %>%  
  read.csv(header = TRUE,stringsAsFactors = F, check.names = F) %>% 
  subset(date %in% limits_dates$t_seq.chr)  %>%
  select(-year, -month, -day) %>% 
  select(contains(codes_catchments),date)

limits_dates = 
  relevant_dates(
  t_ini.chr = Data_Q$date %>% min,
  t_fin.chr = Data_Q$date %>% max
  )
Data_Q = Data_Q %>% select(-date)

Data_Q_mes.f = Data_Q %>% 
  fill_data_daily_pca_to_monthly(limits_dates,codes_catchments) %>% 
  as.data.frame() %>% 
  mutate(MES = as.character(MES)) %>% 
  setnames(old = c("WY","MES","codes_catchments","Q"),
           new=c("wy_simple","month","cod_cuenca","Q_mm")) %>% 
  mutate(wym = month_to_wym(month)) %>% 
  mutate(wy_simple = as.numeric(as.character(wy_simple)))

# export useful file for the next steps
setwd(directory_export)
feather::write_feather(Data_attr,"data_input/attributes_49catchments_ChileCentral.feather")
feather::write_feather(Data_Q_mes.f,"data_input/flows_mm_monthly_49catchments_ChileCentral.feather")