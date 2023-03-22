rm(list = ls())
 
source("base/Preprocess_data.R")
library("foreach")
library("ggplot2")


join_x_info <- function(x,normalised = F) {
  x_train = rownames_to_column(x[["X_train"]],var = "wy") 
  info = x[["info"]]
  df = x_train %>% 
    data.table() %>% 
    melt.data.table(id.vars = c("wy"),
                    variable.name = "predictor_name",
                    value.name = "predictor_value") %>% 
    mutate(catchment_code = info$catchment_code,
           datetime_initialisation = info$datetime_initialisation)
    
  
  return(df)
  
}




# Set catchment code and months of initialization
all_predictors <- grid_pred(c("SOI", "PDO", "ONI","NINO1.2"), 1, "mean")
all_predictors <- c(all_predictors, grid_pred(c("STORAGE"), 1, "last"))

#all available catchments, no data 6008005, 7317005, 7355002, 8106001
catchments_attributes_filename = "data_input/attributes/attributes_49catchments_ChileCentral.csv" 
attributes_catchments = read.csv(catchments_attributes_filename)[-c(32,40,45,49),]
cod_cuencas = attributes_catchments$cod_cuenca

months_initialisation = 1:12

#dataset_data_input <- function(dataset_hydro,normalised = T) {

  model_data <-
    foreach(catchment_code=cod_cuencas, .combine = "c") %do% {
      foreach(month_initialisation = months_initialisation)%do% {
      
      preprocess_data(
        datetime_initialisation = make_date(2022, 3) %m+% months(month_initialisation) ,
        forecast_mode = "cv",
        catchment_code = catchment_code,
        predictor_list = all_predictors,
        save_raw = F
      )

      }
    }
  

months_wy <- c("abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic", "ene", "feb", "mar")
  
data_input = 
  lapply(model_data, 
         function(x) join_x_info(x,normalised = F)) %>% 
rbindlist() %>% 
  merge(attributes_catchments,
        by.x = "catchment_code",
        by.y = "cod_cuenca"
  ) %>%
  #mutate(wy = as.numeric(wy)) %>% 
  mutate(var = tstrsplit(predictor_name, "_", fixed = TRUE)[[1]]) %>% 
  mutate(month_wy = as.factor(wateryearmonth(month = month(datetime_initialisation))))
levels(data_input$month_wy) = paste0("1˚", months_wy[as.numeric(levels(data_input$month_wy) )])


data_input_stat = 
  aggregate(
    x = predictor_value  ~ catchment_code + var + month_wy ,
      data  = data_input,
        FUN = median
    ) %>%
  dplyr::rename(median = predictor_value)

all_data = merge(data_input,data_input_stat,by = c("catchment_code","var","month_wy"))

write.csv(x = all_data,file = "data_output/predictores/all_predictor_1981_2019_45cuencas.csv",row.names = F)

# ##chart
# DGA_code = sample(cod_cuencas,1)
# target_wy = "2016"
# data_input = subset(all_data, catchment_code == DGA_code)
# 
# ggplot(data = data_input) +
#   #add historical records
#   geom_line(aes(x = month_wy, y = predictor_value, col = "1981-2020", group = wy)) +
#   scale_x_discrete(expand = c(0, 0))+
#   #add median
#   geom_line(aes(x = month_wy, y = median, col = "median",group = "wy"))+
# 
#   #add a target year
#   geom_line(data = subset(data_input,wy == target_wy),
#             aes(x = month_wy, y = predictor_value, col = "target_wy",group = "wy"))+
#   scale_color_manual(values = c("median" = "red",
#                                 "1981-2020" = "grey50",
#                                 "target_wy" = "blue"),
#                      labels=c('Median', '1981-2019', target_wy))+
#   facet_wrap(~var, scales = "free_y",ncol = 1)+
#   labs(
#     title = "Predictores",
#     x = "fecha de emisión",
#     y = "valor del predictor",
#     col = "Años hidrológicos"
#   )
