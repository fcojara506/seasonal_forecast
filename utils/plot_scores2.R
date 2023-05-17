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
  
  # df3 <- scores_data$scores_cv3
  # df_ref3 <- scores_data$scores_ref_cv3
  # 
  # df5 <- scores_data$scores_cv5
  # df_ref5 <- scores_data$scores_ref_cv5
  
  df_comb1 <- merge.data.table(df1, df_ref1,
                               by = c("catchment_code", "month_initialisation", "metric_name"),
                               suffixes = c("_best", "_ref")) %>% 
    mutate(resampling = "Leave 1 out")
  
  # df_comb3 <- merge.data.table(df3, df_ref3,
  #                              by = c("catchment_code", "month_initialisation", "metric_name"),
  #                              suffixes = c("_best", "_ref")) %>%
  #   mutate(resampling = "Leave 3 out")
  # 
  # df_comb5 <- merge.data.table(df5, df_ref5,
  #                              by = c("catchment_code", "month_initialisation", "metric_name"),
  #                              suffixes = c("_best", "_ref")) %>%
  #   mutate(resampling = "Leave 5 out")
  # 
  # df_comb <- rbind(df_comb1, df_comb3)
  
  return(df_comb1)
}

read_attributes_catchments <- function(file_path) {
  attributes_catchments <- read_feather(file_path) %>% 
    mutate(cod_cuenca = as.numeric(cod_cuenca)) %>% 
    subset(!(cod_cuenca %in% c(7381001, 4531002, 4522002, 4515002)))
  
  return(attributes_catchments)
}

# Main script
files <- c("data_output/scores/RDS/scores_20230331.RDS",
           "data_output/scores/RDS/scores_reference_20230331.RDS"
           # "data_output/scores/RDS/scores_20230331_cv3k.RDS",
           # "data_output/scores/RDS/scores_reference_20230331_cv3k.RDS",
           # "data_output/scores/RDS/scores_20230331_cv5k.RDS",
           # "data_output/scores/RDS/scores_reference_20230331_cv5k.RDS"
           )

names(files) <- c("scores_loocv", "scores_ref_loocv",
                  "scores_cv3", "scores_ref_cv3",
                  "scores_cv5", "scores_ref_cv5"
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

ggsave(filename = "data_output/figuras/scores/RMSE_best_ref.png",
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

ggsave(filename = "data_output/figuras/scores/R2_best_ref.png",
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

ggsave(filename = "data_output/figuras/scores/MAE_best_ref.png",
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
ggsave(filename = "data_output/figuras/scores/pbias_best_ref.png",
       width = 7,height = 4, plot = p4)





#plot of CRPSS respect to the storage (initial condition)
p5 = ggplot(data = df_crpss)+
  geom_boxplot(aes(x = month_initialisation,
                   y = crpss_storage))+
  labs(title = "CRPSS L1OCV de los volúmenes para distintas fechas de inicialización",
       x = "fecha de emisión",
       y = "CRPSS [-] respecto a la CHI")

ggsave(filename = "data_output/figuras/scores/crpss_ref.png",
       width = 7,height = 4, plot = p5)


#plot of CRPSS respect to the storage (initial condition)
p6=ggplot(data = df_crpss_avg)+
  geom_boxplot(aes(x = month_initialisation,
                   y = value,
                   color = version_sampling
                   ))+
  #scale_color_manual(values = c("red","blue"),labels = c("Mejor combinación", "Referencia"))+
  labs(title = "CRPSS de los volúmenes para distintas fechas de inicialización",
       x = "fecha de emisión",
       y = "CRPSS [-] respecto a volumen climatológico",
       color = "versión"
       )+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(ncol = 2))
print(p6)
ggsave(filename = "data_output/figuras/scores/crpss_climatologico_ref_best.png",
       width = 7,height = 4, plot = p6)


 


### por propiedades de la cuenca

# Create a new variable 'aridity_group' with bins of 1
df_crpss_avg <- df_crpss_avg %>%
  mutate(aridity_group = cut(aridity_cr2met_1979_2010, 
                             breaks = seq(0, max(aridity_cr2met_1979_2010) + 1, by = 1),
                             include.lowest = TRUE, 
                             right = FALSE,
                             labels = paste0(seq(0, max(aridity_cr2met_1979_2010), by = 1), " - ", 
                                             seq(1, max(aridity_cr2met_1979_2010) + 1, by = 1))))







# Create the boxplot
  labs(title = "CRPSS de los volúmenes vs aridez",
       x = "mes de emisión",
       y = "CRPSS [-] respecto al caso sólo CHI",
       fill = "Índice de aridez [-]") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(col = guide_legend(ncol = 2))
  



# Create the boxplot
p7 <- ggplot(data = subset(df_crpss_avg, version_sampling == "Mejor combinación Leave 1 out")) +
  geom_boxplot(aes(x =  month_initialisation,
                   y = value,
                   fill = aridity_group  )) +
  labs(title = "CRPSS de los volúmenes vs aridez",
       x = "mes de emisión",
       y = "CRPSS [-] respecto al caso sólo CHI",
       fill = "Índice de aridez [-]") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(col = guide_legend(ncol = 2))
print(p7)
# Save the plot
ggsave(filename = "data_output/figuras/scores/crpss_best_aridity.png",
       width = 7, height = 4, plot = p7)


# Create a new variable 'hfd' with bins of 1
bin = 0.2
df_crpss_avg <- df_crpss_avg %>%
  mutate(hfd_group = cut(hfd_mean, 
                         breaks = seq(0, max(hfd_mean,na.rm = T) + bin, by = bin),
                         include.lowest = TRUE, 
                         right = FALSE,
                         labels = paste0(seq(0, max(hfd_mean,na.rm = T), by = bin), " - ", 
                                         seq(bin, max(hfd_mean,na.rm = T) + bin, by = bin))))

# Create the boxplot
p8 <- ggplot(data = subset(df_crpss_avg, version_sampling == "Mejor combinación Leave 1 out")) +
  geom_boxplot(aes(x =  month_initialisation,
                   y = value,
                   fill = hfd_group)) +
  labs(title = "CRPSS de los volúmenes vs centroide del hidrograma",
       x = "mes de emisión",
       y = "CRPSS [-] respecto al caso sólo CHI",
       fill = "Centroide del hidrograma [-]") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(col = guide_legend(ncol = 2))
print(p8)
# Save the plot
ggsave(filename = "data_output/figuras/scores/crpss_best_HFD.png",
       width = 7, height = 4, plot = p8)




#precipitation media anual
# Create a new variable 'precipitation_group' with bins of 500mm
df_crpss_avg <- df_crpss_avg %>%
  mutate(precipitation_group = cut(p_mean_cr2met_1979_2010, 
                                   breaks = seq(0, max(p_mean_cr2met_1979_2010) + 500, by = 500),
                                   include.lowest = TRUE, 
                                   right = FALSE,
                                   labels = paste0(seq(0, max(p_mean_cr2met_1979_2010), by = 500), " - ", 
                                                   seq(500, max(p_mean_cr2met_1979_2010) + 500, by = 500), " mm")))

p9 = ggplot(data = subset(df_crpss_avg, version_sampling == "Mejor combinación Leave 1 out")) +
  geom_boxplot(aes(x =  month_initialisation,
                   y = value,
                   fill = precipitation_group)) +
  
  labs(title = "CRPSS de los volúmenes vs precipitación media anual",
       fill = "Precipitación media anual (mm)",
       y = "CRPSS [-] respecto al caso sólo CHI",
       x = "mes de emisión") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill = guide_legend(ncol = 2))

print(p9)
ggsave(filename = "data_output/figuras/scores/crpss_best_precipitation.png",
       width = 7,height = 4, plot = p9)

# Create a new variable 'runoff ratio' with bins of 1
bin = 25
df_crpss_avg <- df_crpss_avg %>%
  mutate(rr_group = cut(runoff_ratio_cr2met_1979_2010, 
                         breaks = seq(0, max(runoff_ratio_cr2met_1979_2010,na.rm = T) + bin, by = bin),
                         include.lowest = TRUE, 
                         right = FALSE,
                         labels = paste0(seq(0, max(runoff_ratio_cr2met_1979_2010,na.rm = T), by = bin), " - ", 
                                         seq(bin, max(runoff_ratio_cr2met_1979_2010,na.rm = T) + bin, by = bin))))

# Create the boxplot
p10 <- ggplot(data = subset(df_crpss_avg, version_sampling == "Mejor combinación Leave 1 out")) +
  geom_boxplot(aes(x =  month_initialisation,
                   y = value,
                   fill = rr_group)) +
  labs(title = "CRPSS de los volúmenes vs coeficiente de escorrentía",
       x = "mes de emisión",
       y = "CRPSS [-] respecto al caso sólo CHI",
       fill = "Coeficiente de escorrentia [-]") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(col = guide_legend(ncol = 2))
print(p10)
# Save the plot
ggsave(filename = "data_output/figuras/scores/crpss_best_runoff_coef.png",
       width = 7, height = 4, plot = p10)


