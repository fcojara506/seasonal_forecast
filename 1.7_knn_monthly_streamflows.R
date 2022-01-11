source("1.0_MAIN.R")

# COUNT THE DAYS IN EACH MONTH
dayofmonth     <- function(mes){days_in_month(as.Date(paste0("2001-",mes,"-01")))}
# NORMALISE BY THE MAX AND MIN
normalizar     <- function(x) { (x-min(x))/(max(x)-min(x))}                     
# COMPUTE THE EUCLIDIAN DISTANCE BETWEEN A MATRIX OF PREDICTORS(Nyears x Npar) AND A REFERENCE VECTOR(1 x Npar)
dist_euclidean <- function(matrix,reference_vector){apply(matrix,1,function(x)sqrt(sum((x-reference_vector)^2)))}
# WEIGHTING FUNCTION BASED ON THE INVERSE DISTANCE
weight         <- function(x){(1/x)/sum(1/x)}                                     
# DISTRIBUTE THE SEASONAL VOLUMEN INTO MONTHLY
seasonal_to_monthly    <- function(seasonal_vector,monthly_distribution){
  monthly_distribution <- data.frame(monthly_distribution)
  prod                 <- rbindlist(lapply(seasonal_vector,function(x) x*monthly_distribution))
  return(prod)
}


funcion_modelo_knn <- function(mes_inicio_str,cuenca_name,N=5,wy_target=2021,predictores_input,version="v2",regression_target) {
  
  
  predictores            <- strsplit(predictores_input,split=' / ', fixed=TRUE) %>% unlist
  version_predictores    <- paste(as.character(predictores), collapse = " / ")
  
  message("Cuenca: ",cuenca_name,", WY: ",wy_target,", MES: ",mes_inicio_str,",Predictor: ",version_predictores)
  
  meses_wy               <- c("abr","may","jun","jul","ago","sep","oct","nov","dic","ene","feb","mar")
  meses_wy_eng           <- c("apr","may","jun","jul","aug","sep","oct","nov","dec","jan","feb","mar")
  
  mes_inicio_num         <- which(tolower(mes_inicio_str)== meses_wy)-1
  seasonal_months_num    <- which(tolower(mes_inicio_str)== meses_wy) %>% seq(12)
  
  
  # ENSEMBLES DE VOLUMENES PRONOSTICADOS
  vols_ens_forecast   <- vol_ens_forecast_input %>%
    subset(wym %in% mes_inicio_num) %>% 
    subset(cuenca %in% cuenca_name) %>% 
    subset(version %in% version_predictores) %>%
    subset(regression_type %in% regression_target) %>% 
    data.table() %>%
    .[,ens := (seq_len(.N)), by=.(wy,version,wym,cuenca,periodo,predictando)] %>%
    reshape2::dcast(formula=...~wy,value.var="volumen") %>%
    select(-c(version,wym,cuenca,periodo,predictando,ens,regression_type))
  
  wy_full_period             <- stringr::str_remove(colnames(vols_ens_forecast),"X") %>% as.numeric()
  wy_training_period         <- wy_full_period[wy_full_period != wy_target]
  selected_months            <- ifelse(mes_inicio_num>5,mes_inicio_num+1,6) %>% seq(.,12) 
  # CAUDALES MEDIOS MENSUALES MEDIDOS
  caudales_mensuales        <- read_feather("data_output/caudal_medio_mensual_1988_presente.feather")%>%
    subset(cuenca %in% cuenca_name) %>% 
    subset(wy %in% wy_training_period) %>% 
    #set_rownames(.$wy) %>%
    select(all_of(selected_months%>%  as.character())) %>%
    `colnames<-`( meses_wy[selected_months] %>% paste0("Q_",.))
  
  caudales_mensual_norm   <- caudales_mensuales/rowSums(caudales_mensuales)
  
  #PREDICTORES DEL PRONOSTICO
  predictores_normalised  <- read_feather("data_output/predictores_v2.feather")%>%
    subset(wy %in% wy_full_period) %>% 
    subset(wym %in% mes_inicio_num) %>% 
    subset(cuenca %in% cuenca_name) %>% 
    select(-cuenca,-wym) %>% 
    structure(row.names = .$wy) %>% 
    select(-wy) %>%
    select(predictores) %>% 
    sapply(normalizar) %>% 
    data.frame() %>%
    mutate(wy=wy_full_period)
  
  predictores_training     <- predictores_normalised %>% filter(wy %in% wy_training_period) %>% select(-wy)
  predictores_target       <- predictores_normalised %>% filter(wy %in% wy_target) %>% select(-wy)
  
  ranking_training_rows    <- order(dist_euclidean(predictores_training,predictores_target))                                                   # Order based on the distance of the parameters
  WY_ranking               <- wy_training_period[ranking_training_rows]
  weight_rank_acum         <- c(0,cumsum(weight(1:N)))                                    # Cumulative Density Funtion (CDF)
  
  # ENSEMBLE OF DATA
  set.seed(10)                                                                           # set the random seed for replicability
  run_numbers              <- 1000                                                          # size of the ensemble
  rows_knn                 <- ranking_training_rows[as.numeric(cut(runif(run_numbers),weight_rank_acum))]                        # Rows of the closest Neighbors in the training matrix
  norm_Q_knn               <- as.matrix(caudales_mensual_norm[rows_knn,])          # 
  
  vol_seasonal_ensemble    <- select(vols_ens_forecast, as.character(wy_target))#*10^6/(86400*30.5) # volumen a caudal medio mensual m3/s
  vol_to_caudal            <- 10^6/86400* 1/dayofmonth(c(4:12,1:3)) %>%
    data.frame()%>%
    t%>%
    data.frame() %>%
    select(stringr::str_to_title(seasonal_months_num %>% meses_wy_eng[.])) %>%
    t
  
  streamflows_monthly_ens  <- seasonal_to_monthly(seasonal_vector = vol_seasonal_ensemble,
                                                  monthly_distribution = norm_Q_knn) %>%
    sweep(MARGIN = 2,vol_to_caudal,`*`) %>%
    mutate(wym=tolower(mes_inicio_str)) %>% 
    mutate(cuenca=cuenca_name) %>% 
    mutate(wy_target=wy_target) %>% 
    mutate(predictores=version_predictores) %>% 
    mutate(ranking=WY_ranking[1]) %>% 
    mutate(version="v2") %>% 
    mutate(regression_type= regression_target) %>%
    data.table()
  
  return(streamflows_monthly_ens)
  
}

##### INPUTS ########

## READ STREAMFLOW AND PARAMETERS DATA FILE

vol_ens_forecast_input <- read_feather("data_output/pronostico_vol_estacional_v2.feather")
predictores_v2         <- vol_ens_forecast_input$version %>% unique()
regression_types       <- vol_ens_forecast_input$regression_type %>% unique()


w                      <- 1
resultados             <- list()

for (regression_target in regression_types) {
for (predictores_input in predictores_v2) {
  for (cuenca_name in cuencas_target) {
    
    resultados[[w]]=
      funcion_modelo_knn(
      mes_inicio_str       = mes_inicio_str,
      cuenca_name          = cuenca_name,
      N                    = n_kNN,
      predictores_input    = predictores_input,
      version              = "v2",
      regression_target    = regression_target
    )
    
    w=w+1
  }
}
}

resultados = resultados %>% rbindlist()
feather::write_feather(resultados,paste0("data_output/caudales_pronosticados_v2.feather"))

