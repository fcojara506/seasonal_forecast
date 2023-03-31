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

merge_scores <- function(scores_data) {
  df1 <- scores_data$scores_loocv
  df_ref1 <- scores_data$scores_ref_loocv
  
  df3 <- scores_data$scores_cv
  df_ref3 <- scores_data$scores_ref_cv
  
  df_comb1 <- merge.data.table(df1, df_ref1,
                               by = c("catchment_code", "month_initialisation", "metric_name"),
                               suffixes = c("_best", "_ref")) %>% 
    mutate(resampling = "loocv")
  
  df_comb3 <- merge.data.table(df3, df_ref3,
                               by = c("catchment_code", "month_initialisation", "metric_name"),
                               suffixes = c("_best", "_ref")) %>%
    mutate(resampling = "cvk3")
  
  df_comb <- rbind(df_comb1, df_comb3)
  
  return(df_comb)
}

read_attributes_catchments <- function(file_path) {
  attributes_catchments <- read_feather(file_path) %>% 
    mutate(cod_cuenca = as.numeric(cod_cuenca)) %>% 
    subset(!(cod_cuenca %in% c(7381001, 4531002, 4522002, 4515002)))
  
  return(attributes_catchments)
}

# Main script
files <- c("data_output/scores/RDS/scores_20230331.RDS",
           "data_output/scores/RDS/scores_reference_20230331.RDS",
           "data_output/scores/RDS/scores_20230331_cv3k.RDS",
           "data_output/scores/RDS/scores_reference_20230331_cv3k.RDS")

names(files) <- c("scores_loocv", "scores_ref_loocv", "scores_cv", "scores_ref_cv")

scores_data <- process_files(files)
df_comb <- merge_scores(scores_data)

attributes_catchments_file <- "data_input/attributes/attributes_49catchments_ChileCentral_more.feather"
attributes_catchments <- read_attributes_catchments(attributes_catchments_file)

df_crpss <- df_comb %>%
  subset(metric_name == "crps_ens") %>%
  mutate(crpss_storage = 1 - (metric_value_best / metric_value_ref)) %>%
  select(c("catchment_code", "month_initialisation", "crpss_storage", "resampling")) %>%
  merge.data.table(attributes_catchments, by.x = "catchment_code", by.y = "cod_cuenca")

df_crpss_avg <- df_comb %>%
  subset(metric_name == "crpss_climatology") %>%
  select(-"metric_name") %>%
  melt.data.table(id.vars = c("catchment_code", "month_initialisation", "resampling")) %>%
  merge.data.table(attributes_catchments, by.x = "catchment_code", by.y = "cod_cuenca")

df_crpss_avg$version <- factor(df_crpss_avg$variable,
                               labels = c("Mejor combinación", "Referencia"))

df_avgens <- df_comb %>%
  subset(!(metric_name %in% c("crps_ens", "crpss_climatology"))) %>%
  dplyr::rename("ref" = "metric_value_ref") %>%
  dplyr::rename("best" = "metric_value_best") %>%
  melt.data.table(id.vars = c("catchment_code", "month_initialisation", "metric_name", "resampling"),
                  variable.name = "version",
                  value.name = "metric_value") %>%
  mutate(version = factor(version,labels = c("Mejor combinación","Referencia"))) %>% 
  mutate(version_sampling = paste(version,resampling)) %>% 
  merge.data.table(attributes_catchments, by.x = "catchment_code", by.y = "cod_cuenca")



           
           


# Load required packages
library(ggplot2)

# Create the plot
p1 <- ggplot(data = subset(df_avgens, metric_name == "rmse_avg")) +
  geom_boxplot(aes(x = month_initialisation,
                   y = metric_value,
                   col = version_sampling)) +
  labs(
    x = "fecha de emisión",
    y = "RMSE/(volumen promedio) [-]",
    col = "Versión",
    title = "Error cuadrático medio normalizado "
  ) +
  theme(legend.position = "bottom",ncol = 2)
  

plot(p1)

ggsave(filename = "data_output/figuras/scores/RMSE_normalizado_best_ref.png",
       width = 7, height = 4, plot = p1)


ggsave(filename = "data_output/figuras/scores/RMSE_normalizado_best_ref.png",
       width = 7, height = 4, plot = p1)



p2 = ggplot(data = subset(df_avgens,metric_name == "r2_avg"))+
  geom_boxplot(aes(x = month_initialisation,
                   y = metric_value,
                   col = version,
                   fill = resampling))+
  labs(
    x = "fecha de emisión",
    y = "R2 [-]",
    col = "Versión",
    title = "Coeficiente de determinación"
  ) + theme(legend.position = "bottom")
plot(p2)
ggsave(filename = "data_output/figuras/scores/R2_best_ref.png",
       width = 7,height = 4,plot = p2)

p3 = ggplot(data = subset(df_avgens,metric_name == "mae_avg"))+
  geom_boxplot(aes(x = month_initialisation,
                   y = metric_value,
                   col = version,
                   fill = resampling))+
  labs(
    x = "fecha de emisión",
    y = "MAE/(volumen promedio) [-]",
    col = "Versión",
    title = "Error absoluto medio (MAE) normalizado"
  ) + theme(legend.position = "bottom")

ggsave(filename = "data_output/figuras/scores/MAE_normalizado_best_ref.png",
       width = 7,height = 4, plot = p3)

p4 = ggplot(data = subset(df_avgens,metric_name == "pbias_avg"))+
  geom_boxplot(aes(x = month_initialisation,
                   y = metric_value,
                   col = version,
                   fill = resampling
                   ))+
  labs(
    x = "fecha de emisión",
    y = "Sesgo porcentual [-]",
    col = "Versión",
    title = "Sesgo porcentual (pBIAS)"
  ) + theme(legend.position = "bottom")

ggsave(filename = "data_output/figuras/scores/pbias_best_ref.png",
       width = 7,height = 4, plot = p4)

# ggplot(data = df_avgens,aes(x = metric_value,fill=month_initialisation))+
#   geom_histogram(aes(y=after_stat(count)/sum(after_stat(count))))+
#   facet_wrap(~metric_name,scales = "free")


# 
# #plot of CRPSS respect to the storage (initial condition)
# p5 = ggplot(data = df_crpss)+
#   geom_boxplot(aes(x = month_initialisation,
#                    y = crpss_storage))+
#   labs(title = "CRPSS de los volúmenes para distintas fechas de inicialización",
#        x = "fecha de emisión",
#        y = "CRPSS [-] respecto al caso sólo CHI")
# 
# ggsave(filename = "data_output/figuras/scores/crpss_ref.png",
#        width = 7,height = 4, plot = p5)


#plot of CRPSS respect to the storage (initial condition)
p6=ggplot(data = df_crpss_avg)+
  geom_boxplot(aes(x = month_initialisation,
                   y = value,
                   color = version,
                   fill = resampling
                   ))+
  #scale_color_manual(values = c("red","blue"),labels = c("Mejor combinación", "Referencia"))+
  labs(title = "CRPSS de los volúmenes para distintas fechas de inicialización",
       x = "fecha de emisión",
       y = "CRPSS [-] respecto a volumén climatológico",
       color = "versión"
       )+
  theme(legend.position = "bottom")

ggsave(filename = "data_output/figuras/scores/crpss_climatologico_ref_best.png",
       width = 7,height = 4, plot = p6)

# ## por latitud
# p7 = ggplot(data = subset(df_crpss_avg, version == "Mejor combinación"))+
#   geom_line(aes(y = value,
#                  x = -gauge_lat,
#                  color = month_initialisation
#   ))+
#   geom_point(aes(y = value,
#                 x = -gauge_lat,
#                 color = month_initialisation
#   ))+
#   #scale_color_manual(values = c("red","blue"),labels = c("Mejor combinación", "Referencia"))+
#   labs(title = "CRPSS de los volúmenes vs latitud de la estación fluviométrica",
#        x = "Latitud (˚S)",
#        y = "CRPSS [-] respecto al caso sólo CHI",
#        color = "mes de emisión"
#   )+
#   theme(legend.position = "bottom")
# ggsave(filename = "data_output/figuras/scores/crpss_best_latitude.png",
#        width = 7,height = 4, plot = p7)
# 
# 
# p8 = ggplot(data = subset(df_crpss_avg, version == "Mejor combinación"))+
#   geom_line(aes(y = value,
#                  x = mean_elev,
#                  color = month_initialisation
#   ))+
#   geom_point(aes(y = value,
#                 x = mean_elev,
#                 color = month_initialisation
#   ))+
#   #scale_color_manual(values = c("red","blue"),labels = c("Mejor combinación", "Referencia"))+
#   labs(title = "CRPSS de los volúmenes vs elevación media de la cuenca",
#        x = "Elevación media de la cuenca (msnm)",
#        y = "CRPSS [-] respecto al caso sólo CHI",
#        color = "mes de emisión"
#   )+
#   theme(legend.position = "bottom")
# 
# ggsave(filename = "data_output/figuras/scores/crpss_best_elevation.png",
#        width = 7,height = 4, plot = p8)
# 
# 
# 
