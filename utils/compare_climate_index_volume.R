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

join_x_info <- function(x,normalised = T) {
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
  months_backwards_list = seq(1,3)
  normalised = F
  
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
    lapply(function(x) join_x_info(x, normalised)) %>% 
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


##################################################################
##                           PLOTTING                           ##
##################################################################
library(tidyr)
library(ComplexHeatmap)
rm(list = ls())
df = readRDS(file = "data_output/scores/RDS/correlations_climates_indices_vol.RDS") %>% data.table()


cor_matrix = df %>% select(catchment_code,month_initialisation,predictor_name,correlation) %>%
  .[, c("var", "fun", "horizon_months") := tstrsplit(predictor_name, "_", fixed = TRUE, keep = 1:3)] %>% 
  mutate(horizon_months = substr(horizon_months,1,1)) %>% 
  select(-predictor_name) %>% 
  select(-fun) %>% 
  tidyr::unite(catchment_month,c(catchment_code,month_initialisation)) %>%
  tidyr::unite(predictor,c(var,horizon_months)) %>%
  dcast(formula = catchment_month ~ predictor,value.var = "correlation") %>% 
  column_to_rownames(var = "catchment_month") %>% 
  as.matrix()

annotation_row = data.table(catchment_month =rownames(cor_matrix)) %>%
  .[, c("catchment_code","month_initialisation") := tstrsplit(catchment_month, "_", fixed = TRUE, keep = 1:2)] %>%
  column_to_rownames(var = "catchment_month") %>% 
  transform(catchment_code = as.numeric(catchment_code))
annotation_row$month_initialisation = factor(annotation_row$month_initialisation,
                                             levels = rev(levels(df$month_initialisation)))

annotation_col = data.table(predictor =colnames(cor_matrix)) %>% 
  .[, c("var","horizon_months") := tstrsplit(predictor, "_", fixed = TRUE, keep = 1:2)] %>%
  column_to_rownames(var = "predictor")

pheatmap(cor_matrix,
         annotation_row = annotation_row,
         annotation_col = annotation_col,
         row_split = annotation_row$month_initialisation,
         column_split = annotation_col$var,
        cluster_row = F,
        cluster_cols = F,
        show_rownames=FALSE)

pheatmap(abs(cor_matrix),
         annotation_row = annotation_row,
         annotation_col = annotation_col,
         row_split = annotation_row$month_initialisation,
         column_split = annotation_col$var,
         cluster_row = F,
         cluster_cols = F,
         show_rownames=FALSE)

# #plot correlation
# p1=ggplot(data = df, 
#        mapping = aes(y = correlation,
#                      col=predictor_name,
#                      x=""))+
#   geom_boxplot()+
#   facet_wrap(~month_initialisation)+
#   labs(
#     title = "Correlación volumen estacional sep-mar, 49 cuencas, 1981-2020",
#     #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
#     x = "",
#     y = "Correlación (Spearman)"
#   )+
#   theme(legend.position = "bottom")
# 
# ggplot2::ggsave("data_output/scores/figures/correlation_vol_climate_indices_per_month.png",
#                 dpi=500,plot = p1, width =  10,height = 6)    
# 
# #plot correlation
# p2=ggplot(data = df, 
#        mapping = aes(x = month_initialisation,
#                      y = correlation))+
#   geom_hline(yintercept = 0)+
#   geom_hline(yintercept = 0.5)+
#   geom_boxplot()+
#   facet_wrap(~predictor_name)+
#   labs(
#     title = "Correlación volumen estacional sep-mar, 49 cuencas, 1981-2020",
#     #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
#     x = "",
#     y = "Correlación (Spearman)"
#   )+
#   theme(legend.position = "bottom")
# plot(p2)
# 
# ggplot2::ggsave("data_output/scores/figures/correlation_vol_per_climate_indices.png",
#                 dpi=500,plot = p2, width =  10,height = 6)    
# 
# #plot correlation
# p3=ggplot(data = df, 
#        mapping = aes(y = floor(gauge_lat),
#                      x = correlation,
#                      group = floor(gauge_lat)))+
#   geom_boxplot()+
#   geom_vline(xintercept = 0)+
#   geom_vline(xintercept = 0.5)+
#   facet_wrap(.~predictor_name)+
#   labs(
#     title = "Correlación volumen estacional sep-mar, 49 cuencas, 1981-2020",
#     #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
#     x = "",
#     y = "Correlación (Spearman)")+
#   theme(legend.position = "bottom")
# 
# plot(p3)
# 
# ggplot2::ggsave("data_output/scores/figures/correlation_vol_climate_indices_per_latitude.png",
#                 dpi=500,plot = p3, width =  10,height = 6)    
# 
# #heatmap correlation
# 
# 
# p4=ggplot(data = df, 
#           mapping = aes(y =as.character(catchment_code),
#                         x = month_initialisation,
#                         fill=correlation))+
#   geom_tile()+
#   facet_wrap( ~predictor_name)+
#   scale_fill_viridis_b()+
#   labs(
#     title = "Correlación (Spearman) volumen estacional sep-mar, 49 cuencas, 1981-2020",
#     #subtitle = "Meteo: pre-processed, averaged 30 ensemble members",
#     x = "Mes inicialización",
#     fill = "Corr",
#     y = "Código cuenca DGA")+
#   theme(legend.position = "right")
# 
# 
# plot(p4)
# 
# ggplot2::ggsave("data_output/scores/figures/correlation_vol_climate_indices_heatmap.png",
#                 dpi=500,
#                 plot = p4,
#                 width =  11,height = 8)    
# 
