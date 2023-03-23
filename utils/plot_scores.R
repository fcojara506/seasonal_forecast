#rm(list = ls())
library(data.table)


join_x_info <- function(x) {
  
  data = x[["scores_volume"]]
  info_x = x[["info"]]
  
  info = 
    info_x[c("predictor_list",
             "predictor_list_corrected")] %>% 
    rbind()
  
  df = data.table(data) %>% 
    cbind(info) %>% 
    mutate(datetime_initialisation = info_x$datetime_initialisation) %>% 
    mutate(catchment_code = info_x$catchment_code)
  
  
  return(df)
  
}

dummy_predictors <- function(variables) {
  predictors = data$predictor_list
  
  # Find the maximum length of the lists in 'predictors'
  max_len <- max(sapply(predictors, length))
  # Extend each list to the maximum length using NA as filler
  extended_predictors <- lapply(predictors, function(x) {
    length(x) <- max_len
    x
  })
  library(tidyverse)
  # Create a data frame
  df <- data.frame(do.call(rbind, extended_predictors),
                   stringsAsFactors = F) %>% 
    mutate(id = row_number()) %>% 
    reshape2::melt(id.var = "id",value.name = "predictor") %>% 
    select(-"variable") %>%
    drop_na("predictor") %>% 
    mutate(var = tstrsplit(predictor, "_", fixed = TRUE)[[1]]) %>% 
    reshape2::acast(id ~ var,value.var = "predictor")
  
  dummy_vars = !is.na(df)
  return(dummy_vars)
}





scores = readRDS(file = "data_output/scores/RDS/scores_20230323.RDS")
data = lapply(scores, join_x_info) %>% rbindlist()
dummy_pred = dummy_predictors()
predictors = colnames(dummy_pred)

dataframe = cbind(data[,-c("predictor_list","predictor_list_corrected")],dummy_pred) %>% 
  data.table() %>%
  melt.data.table(
     id.vars = c("catchment_code","datetime_initialisation"),
     measure.vars = predictors
     )%>% 
  mutate(month_initialisation = month(datetime_initialisation))

library("ggplot2")
library("sf")
shapefile_path = "data_input/SIG/shapefile_cuencas/cuencas_fondef-dga.shp"
# Read the shapefile
shapefile <- st_read(shapefile_path,quiet = T)
shapefile <- st_make_valid(shapefile)
shapefile <- st_simplify(shapefile, dTolerance = 4000)

# Merge the shapefile and the dataframe using the common ID
merged_data <- merge(shapefile,
                     dataframe,
                     by.x = "gauge_id",
                     by.y = "catchment_code") 

# Plot the metric using the merged data
#plot <- 
  ggplot() +
  geom_sf(data = merged_data, aes(fill = value)) +
  #scale_fill_continuous(low = "blue", high = "red") + # Change the colors according to your preference
  theme_minimal() +
  facet_grid(month_initialisation ~ variable) +
    scale_x_continuous(breaks = seq(-65,-75,by = -2),labels = seq(-65,-75,by = -2)) +
    scale_y_continuous(breaks = seq(-27, -37, by = -2),labels = seq(-27, -37, by = -2))+
    labs(title = "", x = "Longitud", y = "Latitud")+
    coord_sf(xlim = c(-65, -75), ylim = c(-27, -37))
  
