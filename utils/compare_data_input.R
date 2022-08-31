rm(list = ls())

library(ggplot2)

source("utils/run_model_function.R")
short_river_name <- function(var) {stringr::word(var,start = 1,end = 2)}

#test catchments
attributes_catchments = 
  "base/data_input/attributes/attributes_49catchments_ChileCentral.feather" %>% 
  feather::read_feather()

cod_cuencas1 = 
  "base/data_input/storage_variables/hydro_variables_monthly_catchments_ChileCentral_GR4J_KGE.feather" %>% 
  feather::read_feather() %>%
  select(cod_cuenca) %>% 
  unique() %>% 
  .[["cod_cuenca"]]

cod_cuencas2 = attributes_catchments$cod_cuenca

cod_cuencas = intersect(cod_cuencas1,cod_cuencas2)
#test months initial
months_initialisation = c("oct") #c('may','jun','jul','ago','sep','oct','nov','dic','ene','feb')

#test meteo set 
datasets_hydro = c(
  "GR4J_KGE","GR4J_NSE","GR4J_EVDSep","GR4J_KGE+logNSE","GR4J_SKGE",
  "TUW_KGE","TUW_NSE","TUW_EVDSep","TUW_KGE+logNSE","TUW_SKGE",
  "SAC_KGE","SAC_NSE","SAC_EVDSep","SAC_KGE+logNSE","SAC_SKGE"
  )
#test forecast order
iterations = length(cod_cuencas)*length(months_initialisation)*length(datasets_hydro)
message("Total iteraciones:", iterations)

library(foreach)
library(doParallel)
#registerDoParallel(cores=8)


   
   

join_x_info <- function(x) {
  x_train = rownames_to_column(x[["X_train"]][[1]],var = "wy") %>% minmax()
  y_train = rownames_to_column(x[["y_train"]],var = "wy") %>% minmax()
  
  
  
  xy_train = merge(x_train,y_train)
  
  
  df = xy_train %>% 
    data.table() %>% 
    melt.data.table(id.vars = c("wy","volume_mm"),
                    variable.name = "predictor_name",
                    value.name = "predictor_value") %>% 
    c(x[["info"]])
    
  
  return(df)
  
}

dataset_data_input <- function(dataset_hydro) {

  model_data <-
    foreach(catchment_code=cod_cuencas) %dopar% {
      
      preprocess_data(
        catchment_code = catchment_code,
        month_initialisation = "sep",
        dataset_hydro = dataset_hydro,
        dataset_region = "ChileCentral",
        dataset_meteo  = "ens30avg",
        horizon_strategy = "dynamic",
        horizon_month_start = "oct",
        horizon_month_end = "mar",
        predictor_list = c(
          "pr_sum_-1months",
          #"tem_mean_-1months",
          "SP_last_1months",
          "STORAGE_last_1months",
          # GR4J
          "PROD_last_1months",
          "ROUT_last_1months",
          "SM_last_1months",
          #TUW
          "SUZ_last_1months",
          "SLZ_last_1months",
          "UZT_last_1months",
          #SACRAMENTO
          "UZF_last_1months",
          "LZT_last_1months",
          "LZS_last_1months",
          "LZP_last_1months"
        ),
        wy_holdout = 2000,
        remove_wys = NA
      )
      
    }  
  
data_input = 
  lapply(model_data,  join_x_info) %>% 
  rbindlist() %>% 
  merge(attributes_catchments,
        by.x = "catchment_code",
        by.y = "cod_cuenca"
  ) %>%
  mutate(short_gauge_name = short_river_name(gauge_name))

return(data_input)
}

minmax <- function(df,method = "range" ) {
  library(caret)
  library(dplyr)
  
  df2 =  df %>% 
    preProcess(method = method) %>% 
    predict(df)
  
}
#saveRDS(data_input,"utils/data_output/model_data_input_49catchments.RDS")
plot_input <- function(dataset_hydro= "TUW_EVDSep") {

data_input = dataset_data_input(dataset_hydro)

data_input_mean = 
  aggregate(
    formula = predictor_value  ~ catchment_code + predictor_name ,
      data  = data_input,
        FUN = mean
    )

ggplot(data = data_input,
       aes(x = predictor_value, y=volume_mm)) +
geom_point()+
geom_smooth(formula = y ~ x,
              fullrange=F,
              method = "loess",
              se = F, 
            size = 0.5
              ) +
geom_vline(data = data_input_mean,
           aes(xintercept = predictor_value))+
facet_grid(catchment_code ~ predictor_name,
           scales = "free")+
  labs(
    x = "Predictor (Almacenamiento) [mm]",
    y = "Volumen estacional [mm]",
    title = "Volumen vs Predictores (forma: Variable_FuncionAgregacion_MesesAgregracion )",
    subtitle = dataset_hydro
  )+
  scale_y_continuous(
    limits = c(0,1)
  )

ggsave(glue::glue("utils/data_output/figuras/xy_{dataset_hydro}.png"),
       width = 12,
       height = 18)
}

sapply(datasets_hydro, plot_input)