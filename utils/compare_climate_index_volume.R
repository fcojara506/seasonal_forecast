rm(list = ls())

library(ggplot2)

source("utils/run_model_function.R")

test <- function() {
  
  x=
  preprocess_data(
    catchment_code = "5200001",
    month_initialisation = "abr",
    horizon_month_start = "oct",
    horizon_month_end = "mar",
    predictor_list = paste(climate_indices,"last",paste0(1,"months"),sep = "_"),
    wy_holdout = 2022,
    remove_wys = NA,
    units_q = "m3/s",
    units_y = "GL",
    test_subset = F
  )
  
  xx = join_x_info(x,normalised = F)
}

minmax <- function(df,method = "range" ) {
  library(caret)
  library(dplyr)
  
  df2 =  df %>% 
    preProcess(method = method) %>% 
    predict(df)
  
}


join_x_info <- function(x,normalised = T) {
  
  x_train = rownames_to_column(x[["X_train"]][[1]],var = "wy") 
  y_train = rownames_to_column(x[["y_train"]],var = "wy")
  
  if (normalised) {
    x_train = x_train %>% minmax(method = "range")
    y_train = y_train %>% minmax(method = "range")
  }
  
  xy_train = merge(x_train,y_train)
  
  df = xy_train %>% 
    data.table() %>% 
    melt.data.table(id.vars = c("wy","volume"),
                    variable.name = "predictor_name",
                    value.name = "predictor_value") %>% 
    c(x[["info"]])
  
  return(df)
  
}

dataset_data_input <- function(months_initialisation,cod_cuencas,normalised = F) {
  
  library(foreach)
  library(doParallel)
  registerDoParallel(cores=parallel::detectCores())
  
  model_data <-
    foreach(month_initialisation=months_initialisation,.combine = "c") %:%
    foreach(catchment_code=cod_cuencas) %dopar% {
      
      preprocess_data(
        catchment_code = catchment_code,
        month_initialisation = month_initialisation,
        horizon_month_start = "sep",
        horizon_month_end = "mar",
        horizon_strategy = "static",
        predictor_list = paste(climate_indices,"last",paste0(1,"months"),sep = "_"),
        wy_holdout = 2022,
        remove_wys = NA,
        units_q = "m3/s",
        units_y = "GL",
        test_subset = F
      )
      
    } 
  
  data_input = lapply(model_data, 
                      function(x) join_x_info(x,normalised = normalised)) %>% 
    rbindlist()
  
  stopImplicitCluster()
  return(data_input)
}

#test catchments
attributes_catchments = "data_input/attributes/attributes_49catchments_ChileCentral.feather" %>% 
  feather::read_feather()

cod_cuencas = attributes_catchments$cod_cuenca
#test months initial
months_initialisation =  c('may','jun','jul','ago','sep','oct','nov','dic','ene','feb','mar')

# climate indices
climate_indices = c(
  "MEIv2",
  "PDO",
  "SOI",
  "ONI",
  "NINO1.2",
  "NINO3",
  "NINO4",
  "NINO3.4",
  "ESPI",
  "AAO",
  "BIENSO"
)

# get input climate indices and volume for each basin, month for many predictors
df_input=dataset_data_input(months_initialisation = months_initialisation,
                     cod_cuencas = cod_cuencas,
                     normalised = F)
#compute correlation of climate indices vs seasonal volume
df = df_input %>%
  group_by(catchment_code,month_initialisation,predictor_name) %>% 
  summarise(correlation = cor(x = volume,y = predictor_value,method = "spearman"))

#add catchment attributes
df = df %>%
  merge(attributes_catchments,
        by.x = "catchment_code",
        by.y = "cod_cuenca") %>% 
  mutate(short_gauge_name = short_river_name(gauge_name))
# order columns
df$month_initialisation = factor(df$month_initialisation,levels = months_initialisation)
df$predictor_name = factor(df$predictor_name,labels = climate_indices)
df$catchment_code = as.numeric(df$catchment_code)


saveRDS(object = df, file = "data_output/scores/RDS/correlations_climates_indices_vol.RDS")

rm(list = ls())
df = readRDS(file = "data_output/scores/RDS/correlations_climates_indices_vol.RDS")
#plot correlation
p1=ggplot(data = df, 
       mapping = aes(y = correlation,
                     col=predictor_name,
                     x=""))+
  geom_boxplot()+
  facet_wrap(~month_initialisation)+
  labs(
    title = "Correlación volumen estacional sep-mar, 49 cuencas, 1981-2020",
    #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
    x = "",
    y = "Correlación (Spearman)"
  )+
  theme(legend.position = "bottom")

ggplot2::ggsave("data_output/scores/figures/correlation_vol_climate_indices_per_month.png",
                dpi=500,plot = p1, width =  10,height = 6)    

#plot correlation
p2=ggplot(data = df, 
       mapping = aes(x = month_initialisation,
                     y = correlation))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5)+
  geom_boxplot()+
  facet_wrap(~predictor_name)+
  labs(
    title = "Correlación volumen estacional sep-mar, 49 cuencas, 1981-2020",
    #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
    x = "",
    y = "Correlación (Spearman)"
  )+
  theme(legend.position = "bottom")
plot(p2)

ggplot2::ggsave("data_output/scores/figures/correlation_vol_per_climate_indices.png",
                dpi=500,plot = p2, width =  10,height = 6)    

#plot correlation
p3=ggplot(data = df, 
       mapping = aes(y = floor(gauge_lat),
                     x = correlation,
                     group = floor(gauge_lat)))+
  geom_boxplot()+
  geom_vline(xintercept = 0)+
  geom_vline(xintercept = 0.5)+
  facet_wrap(.~predictor_name)+
  labs(
    title = "Correlación volumen estacional sep-mar, 49 cuencas, 1981-2020",
    #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
    x = "",
    y = "Correlación (Spearman)")+
  theme(legend.position = "bottom")

plot(p3)

ggplot2::ggsave("data_output/scores/figures/correlation_vol_climate_indices_per_latitude.png",
                dpi=500,plot = p3, width =  10,height = 6)    

#heatmap correlation


p4=ggplot(data = df, 
          mapping = aes(y =as.character(catchment_code),
                        x = month_initialisation,
                        fill=correlation))+
  geom_tile()+
  facet_wrap( ~predictor_name)+
  scale_fill_viridis_b()+
  labs(
    title = "Correlación (Spearman) volumen estacional sep-mar, 49 cuencas, 1981-2020",
    #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
    x = "Mes inicialización",
    fill = "Corr",
    y = "Código cuenca DGA")+
  theme(legend.position = "right")


plot(p4)

ggplot2::ggsave("data_output/scores/figures/correlation_vol_climate_indices_heatmap.png",
                dpi=500,
                plot = p4,
                width =  11,height = 8)    

