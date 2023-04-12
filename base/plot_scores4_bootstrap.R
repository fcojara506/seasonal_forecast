# Clear workspace
rm(list = ls())

# Load libraries
library(dplyr)
library(data.table)
library(feather)

# Define functions
join_x_info <- function(x) {
  data <- x[["scores_volume"]]
  info_x <- x[["info"]]
  predictor_list <- rbind(info_x[c("predictor_list")])
  
  months_es <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
  
  df <- data.table(data) %>% 
    cbind(predictor_list) %>%
    mutate(month_initialisation = paste0("1˚", months_es[(month(info_x$datetime_initialisation))])) %>% 
    mutate(catchment_code = info_x$catchment_code)
  
  return(df)
}

sort_months <- function(scores) {
  months_wy <- c("abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic", "ene", "feb", "mar")
  scores$month_initialisation <- factor(scores$month_initialisation, levels = paste0("1˚", months_wy))
  
  return(scores)
}

read_and_process_scores <- function(file_path) {
  scores <- readRDS(file = file_path) %>% 
    lapply(join_x_info) %>% 
    rbindlist() %>% 
    sort_months() %>%
    data.table() %>%
    select(-c("predictor_list", "mae_obs")) %>%
    melt.data.table(id.vars = c("catchment_code", "month_initialisation"),
                    variable.name = "metric_name", value.name = "metric_value")
  
  return(scores)
}

process_files <- function(file_paths) {
  scores_data <- lapply(file_paths, read_and_process_scores)
  return(scores_data)
}



read_attributes_catchments <- function(file_path) {
  attributes_catchments <- read_feather(file_path) %>% 
    mutate(cod_cuenca = as.numeric(cod_cuenca)) %>% 
    subset(!(cod_cuenca %in% c(7381001, 4531002, 4522002, 4515002)))
  
  return(attributes_catchments)
}

merge_scores <- function(scores_data) {
  df1 <- scores_data$scores_loocv
  df_ref1 <- scores_data$scores_ref_loocv
  
  df3 <- scores_data$scores_boot
  df_ref3 <- scores_data$scores_ref_boot
  
  
  df_comb1 <- merge.data.table(df1, df_ref1,
                               by = c("catchment_code", "month_initialisation", "metric_name"),
                               suffixes = c("_best", "_ref")) %>% 
    mutate(resampling = "normal")
  
  df_comb3 <- merge.data.table(df3, df_ref3,
                               by = c("catchment_code", "month_initialisation", "metric_name"),
                               suffixes = c("_best", "_ref")) %>%
    mutate(resampling = "bootstrap")
  
  df_comb <- rbind(df_comb1, df_comb3)
  
  return(df_comb)
}

# Main script
files <- c("data_output/scores/RDS/scores_20230331.RDS",
           "data_output/scores/RDS/scores_reference_20230331.RDS",
           "data_output/scores/RDS/scores_20230411_bag.RDS",
           "data_output/scores/RDS/scores_reference_20230411_bag.RDS"
)



names(files) <- c("scores_loocv", "scores_ref_loocv",
                  "scores_boot", "scores_ref_boot"
)



scores_data <- process_files(files)

df_comb <- merge_scores(scores_data)

attributes_catchments_file <- "data_input/attributes/Cuencas_Fondef-DGA_v1.csv"
attributes_catchments <- fread(attributes_catchments_file)

df_crpss <- df_comb %>%
  subset(resampling == "Leave 1 out") %>% 
  subset(metric_name == "crps_ens") %>%
  mutate(crpss_storage = 1 - (metric_value_best / metric_value_ref)) %>%
  select(c("catchment_code", "month_initialisation", "crpss_storage", "resampling")) %>% 
  merge.data.table(attributes_catchments, by.x = "catchment_code", by.y = "gauge_id")

df_crpss_avg <- df_comb %>%
  subset(metric_name == "crpss_climatology") %>%
  select(-"metric_name") %>%
  melt.data.table(id.vars = c("catchment_code", "month_initialisation", "resampling")) %>%
  merge.data.table(attributes_catchments, by.x = "catchment_code", by.y = "gauge_id") %>% 
  mutate(version=factor(variable,labels = c("Mejor combinación", "Referencia"))) %>%
  mutate(version_sampling = paste(version,resampling))

df_avgens <- df_comb %>%
  subset(!(metric_name %in% c("crps_ens", "crpss_climatology"))) %>%
  dplyr::rename("ref" = "metric_value_ref") %>%
  dplyr::rename("best" = "metric_value_best") %>% 
  melt.data.table(id.vars = c("catchment_code", "month_initialisation", "metric_name", "resampling"),
                  variable.name = "version",
                  value.name = "metric_value") %>%
  mutate(version = factor(version,labels = c("Mejor combinación","Referencia"))) %>% 
  mutate(version_sampling = paste(version,resampling)) %>% 
  merge.data.table(attributes_catchments, by.x = "catchment_code", by.y = "gauge_id")



# Load required packages
library(ggplot2)

# Create the plot
p1 <- ggplot(data = subset(df_avgens, metric_name == "rmse_avg")) +
  geom_boxplot(aes(x = month_initialisation,
                   y = metric_value,
                   col = version_sampling)) +
  labs(
    x = "fecha de emisión",
    y = "RMSE [mill m3]",
    col = "Versión",
    title = "Error cuadrático medio "
  ) +
  theme(legend.position = "bottom")+
  guides(col=guide_legend(ncol=2))


plot(p1)
ggsave(filename = "data_output/figuras/scores/RMSE_best_ref_bagging.png",
       width = 7, height = 4, plot = p1)


p2 = ggplot(data = subset(df_avgens,metric_name == "r2_avg"))+
  geom_boxplot(aes(x = month_initialisation,
                   y = metric_value,
                   col = version_sampling))+
  labs(
    x = "fecha de emisión",
    y = "R2 [-]",
    col = "Versión",
    title = "Coeficiente de determinación"
  ) + theme(legend.position = "bottom")+
  guides(col=guide_legend(ncol=2))

plot(p2)
ggsave(filename = "data_output/figuras/scores/R2_best_ref_bagging.png",
       width = 7,height = 4,plot = p2)


p3 = ggplot(data = subset(df_avgens,metric_name == "mae_avg"))+
  geom_boxplot(aes(x = month_initialisation,
                   y = metric_value,
                   col = version_sampling))+
  labs(
    x = "fecha de emisión",
    y = "MAE [mill m3]",
    col = "Versión",
    title = "Error absoluto medio (MAE)"
  ) + theme(legend.position = "bottom")+
  guides(col=guide_legend(ncol=2))
print(p3)

ggsave(filename = "data_output/figuras/scores/MAE_best_ref_bagging.png",
       width = 7,height = 4, plot = p3)

p4 = ggplot(data = subset(df_avgens,metric_name == "pbias_avg"))+
  geom_boxplot(aes(x = month_initialisation,
                   y = metric_value,
                   col = version_sampling
  ))+
  labs(
    x = "fecha de emisión",
    y = "Sesgo porcentual [-]",
    col = "Versión",
    title = "Sesgo porcentual (pBIAS)"
  ) + theme(legend.position = "bottom")+
  guides(col=guide_legend(ncol=2))
print(p4)
ggsave(filename = "data_output/figuras/scores/pbias_best_ref_bagging.png",
       width = 7,height = 4, plot = p4)








#plot of CRPSS respect to the storage (initial condition)
p6=ggplot(data = df_crpss_avg)+
  geom_boxplot(aes(x = month_initialisation,
                   y = value,
                   color = version_sampling
  ))+
  #scale_color_manual(values = c("red","blue"),labels = c("Mejor combinación", "Referencia"))+
  labs(title = "CRPSS de los volúmenes para distintas fechas de inicialización",
       x = "fecha de emisión",
       y = "CRPSS [-] respecto a volumén climatológico",
       color = "versión"
  )+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(ncol = 2))
print(p6)





