rm(list = ls())

library(ggplot2)

source("utils/run_model_function.R")

minmax <- function(df) {
  library(caret)
  library(dplyr)
  df2 =  preProcess(df,method = "range" ) %>%
    predict(df)
}

convert_items_to_lists <- function(lst) {
  lst <- lapply(lst, function(x) if (length(x) > 1) list(x) else x)
  return(lst)
}

join_x_info <- function(x) {
  #get online x and y training set
  x_train = rownames_to_column(x$X_train,var = "wy") 
  y_train = rownames_to_column(x$y_train,var = "wy")
  #merge
  xy_train = merge(x_train,y_train)
  # convert items with length>1 on lists
  x$info = convert_items_to_lists(x$info)
  #transform x and y and add info
  df = xy_train %>% 
    data.table() %>% 
    melt.data.table(id.vars = c("wy","volume"),
                    variable.name = "predictor_name",
                    value.name = "predictor_value") %>% 
    c(x[["info"]])
  
  return(df)
  
}


#test catchments
attributes_catchments = "data_input/attributes/attributes_49catchments_ChileCentral.feather" %>%
  feather::read_feather()

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

  months_initialisation =  c('abr','may','jun','jul','ago')
  cod_cuencas = attributes_catchments$cod_cuenca
  months_backwards_list = seq(1,5)

  ###### code
  
  library(foreach)
  library(doParallel)
  registerDoParallel(cores=parallel::detectCores())
  
  model_data <- 
  foreach(month_initialisation=months_initialisation,.combine = "c") %:%
  foreach(months_backwards=months_backwards_list,.combine = "c") %:%
  foreach(catchment_code=cod_cuencas) %dopar% {
    
        predictors = paste(climate_indices,"mean",paste0(months_backwards,"months"),sep = "_")
        
        preprocess_data(
          catchment_code = catchment_code,
          month_initialisation = month_initialisation,
          horizon_month_start = "sep",
          horizon_month_end = "mar",
          horizon_strategy = "static",
          predictor_list = predictors,
          wy_holdout = 2022,
          remove_wys = seq(1950,1980),
          units_q = "m3/s",
          units_y = "GL",
          test_subset = F
        )
        
      }

  data_input = model_data %>%
    lapply(function(x) join_x_info(x)) %>% 
    rbindlist()
  
  stopImplicitCluster()
  
  ###### code plotting
  
#compute correlation of climate indices vs seasonal volume
df = data_input %>%
  group_by(catchment_code,month_initialisation,predictor_name) %>% 
  summarise(correlation = cor(x = volume, y = predictor_value, method = "spearman"))
#add catchment attributes


# order columns
df$month_initialisation = factor(df$month_initialisation,levels = months_initialisation)
df$predictor_name = factor(df$predictor_name)
df$catchment_code = as.numeric(df$catchment_code)

saveRDS(object = df, file = "data_output/scores/RDS/correlations_climates_indices_vol.RDS")

