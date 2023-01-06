rm(list = ls())

library(ggplot2)

source("utils/run_model_function.R")

test <- function() {
  
  x=
  preprocess_data(
    catchment_code = "5200001",
    month_initialisation = "dic",
    horizon_month_start = "oct",
    horizon_month_end = "mar",
    predictor_list = paste(climate_indices,"mean",paste0(1,"months"),sep = "_"),
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
        predictor_list = paste(climate_indices,"mean",paste0(1,"months"),sep = "_"),
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
months_initialisation =  c('may','jun','jul','ago','sep','oct','nov','dic','ene','feb')

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
#plot correlation
ggplot(data = df, 
       mapping = aes(y = correlation,
                     col=predictor_name))+
  geom_boxplot()+
  facet_wrap(~month_initialisation)

#plot correlation
ggplot(data = df, 
       mapping = aes(x = month_initialisation,
                     y = correlation))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.5)+
  geom_boxplot()+
  facet_wrap(~predictor_name)

#plot correlation
ggplot(data = df, 
       mapping = aes(y = floor(gauge_lat),
                     x = correlation,
                     group = floor(gauge_lat)))+
  geom_boxplot()+
  geom_vline(xintercept = 0)+
  facet_wrap(.~predictor_name)
