# Clear workspace
rm(list = ls())

# Load libraries
library(dplyr)
library(data.table)
library(feather)
library(sf)
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
  
  df3 <- scores_data$scores_cv3
  df_ref3 <- scores_data$scores_ref_cv3
  
  df5 <- scores_data$scores_cv5
  df_ref5 <- scores_data$scores_ref_cv5
  
  df_comb1 <- merge.data.table(df1, df_ref1,
                               by = c("catchment_code", "month_initialisation", "metric_name"),
                               suffixes = c("_best", "_ref")) %>% 
    mutate(resampling = "Leave 1 out")
  
  df_comb3 <- merge.data.table(df3, df_ref3,
                               by = c("catchment_code", "month_initialisation", "metric_name"),
                               suffixes = c("_best", "_ref")) %>%
    mutate(resampling = "Leave 3 out")
  
  df_comb <- rbind(df_comb1, df_comb3)
  
  return(df_comb)
}

read_attributes_catchments <- function(file_path) {
  attributes_catchments <- read_feather(file_path) %>% 
    mutate(cod_cuenca = as.numeric(cod_cuenca)) %>% 
    subset(!(cod_cuenca %in% c(7381001, 4531002, 4522002, 4515002)))
  
  return(attributes_catchments)
}

target_date <- "20230410"

# Main script
files <- c("data_output/scores/RDS/scores_20230331.RDS",
           "data_output/scores/RDS/scores_reference_20230331.RDS",
           "data_output/scores/RDS/scores_20230331_cv3k.RDS",
           "data_output/scores/RDS/scores_reference_20230331_cv3k.RDS"
)

# Modify file names using the target_date variable
files <- gsub("20230331", target_date, files)

names(files) <- c("scores_loocv", "scores_ref_loocv",
                  "scores_cv3", "scores_ref_cv3"
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
library(gridExtra)

levels(df_crpss_avg$version) = c("SWE+almacenamientos & índices climáticos","SWE+almacenamientos")

#plot of CRPSS respect to the storage (initial condition)
p = ggplot(data = df_crpss_avg %>% subset(resampling == "Leave 1 out"))+
  geom_boxplot(aes(x = month_initialisation,
                   y = value,
                   color = version
  ))+
  geom_hline(yintercept =  0)+
  #scale_color_manual(values = c("red","blue"),labels = c("Mejor combinación", "Referencia"))+
  labs(title = "CRPSS de los volúmenes para distintas fechas de inicialización",
       x = "fecha de emisión",
       y = "CRPSS [-] c/r volumen promedio 1981-2019",
       color = "",
       caption = "Cada boxplot agrupa 45 cuencas"
  )+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(ncol = 1)) +
  scale_color_brewer(palette = "Set1",direction = -1)+
  ylim(NA,1)

print(p)
ggsave(filename = "data_output/figuras/scores/crpss_climatologico_ref_best_L1OCV.png",
       width = 7,height = 4,dpi = 400, plot = p)

levels(df_avgens$version) = c("SWE+almacenamientos & índices climáticos","SWE+almacenamientos")

p2 = ggplot(data = subset(df_avgens,metric_name == "r2_avg")%>% subset(resampling == "Leave 1 out"))+
  geom_boxplot(aes(x = month_initialisation,
                   y = metric_value,
                   col = version))+
  labs(
    x = "fecha de emisión",
    y = "R2 [-]",
    col = "",
    title = "R2 del volumen obs vs promedio del pronóstico",
    caption = "Cada boxplot agrupa 45 cuencas"
  ) + theme(legend.position = "bottom")+
  guides(col=guide_legend(ncol=2))+  geom_hline(yintercept =  0)+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(ncol = 1)) +
  scale_color_brewer(palette = "Set1",direction = -1)+
  ylim(NA,1)

plot(p2)

ggsave(filename = "data_output/figuras/scores/r2_ref_best_L1OCV.png",
       width = 7,height = 4,dpi = 400, plot = p2)


plot_metric <- function(dataframe,
                        metric,
                        metric_name = metric,
                        by_x = "gauge_id",
                        by_y = "gauge_id",
                        all_x = T,
                        shapefile_path = "data_input/SIG/shapefile_cuencas/cuencas_fondef-dga.shp") {
  # Read the shapefile
  shapefile <- read_sf(shapefile_path)
  shapefile <- st_make_valid(shapefile)
  shapefile <- st_simplify(shapefile, dTolerance = 2000)
  
  # Merge the shapefile and the dataframe using the common ID
  merged_data <- merge(shapefile, dataframe, by.x = by_x, by.y = by_y,all.x=all_x)
  
  # Plot the metric using the merged data
  plot <- ggplot() +
    geom_sf(data = merged_data, aes(fill = !!sym(metric))) +
    scale_fill_continuous(high = "blue", low = "red") + # Change the colors according to your preference
    theme_void() +
    labs(title = "", x = "Longitud", y = "Latitud", fill = metric_name)
  
  return(plot)
}

plot_metric2 <- function(dataframe,
                        metric,
                        metric_name = metric,
                        by_x = "gauge_id",
                        by_y = "gauge_id",
                        all_x = T,
                        shapefile_path = "data_input/SIG/shapefile_cuencas/cuencas_fondef-dga.shp") {

  # Read the shapefile
  shapefile <- read_sf(shapefile_path)
  shapefile <- st_make_valid(shapefile)
  shapefile <- st_simplify(shapefile, dTolerance = 2000)
  
  # Merge the shapefile and the dataframe using the common ID
  merged_data <- merge(shapefile, dataframe, by.x = by_x, by.y = by_y,all.x=all_x)
  
  # Plot the metric using the merged data
  plot <- ggplot() +
    geom_sf(data = merged_data, aes(fill = !!sym(metric))) +
    scale_fill_continuous(high = "blue", low = "red") + # Change the colors according to your preference
    theme_void() +
    labs(x = "Longitud", y = "Latitud", title = metric_name,fill = "")
  
  return(plot)
}

p_aridez = plot_metric(attributes_catchments,
            metric = "aridity_cr2met_1979_2010",
            metric_name = "")
p_hfd = plot_metric(attributes_catchments,
                       metric = "hfd_mean",
                       metric_name = "")
p_pmean = plot_metric(attributes_catchments,
                       metric = "p_mean_cr2met_1979_2010",
                       metric_name = "")

p_runoffratio = plot_metric(attributes_catchments,
                       metric = "runoff_ratio_cr2met_1979_2010",
                       metric_name = "")

# p1 <- ggplot(data = subset(df_crpss_avg, version_sampling == "Mejor combinación Leave 1 out")) +
#   geom_point(aes(  x =  aridity_cr2met_1979_2010 ,
#                    y = value,
#                    col = month_initialisation))+
#   facet_wrap(~month_initialisation) +
#   labs(x = "Indice aridez (P/PET) [mm/mm]",
#        y = "CRPSS c/r volumen promedio",
#        title = "CRPSS vs índice de aridez del modelo 'mejor combinación (AIC)' ",
#        col = "mes emisión")+
#   theme(legend.position = "bottom") +
#   guides(col = guide_legend(nrow = 2))
# p1 = grid.arrange(p1,p_aridez,ncol =2,widths = c(3,1))
# 
# print(p1)

df = subset(df_crpss_avg, month_initialisation %in% c("1˚jul","1˚sep") &
         version_sampling == "Mejor combinación Leave 1 out" & value<0.3)

p_low = plot_metric(df,
                       metric = "value",
                        by_y = "catchment_code",
                       metric_name = "")
print(p_low)
ggsave(filename = "data_output/figuras/scores/cuencas_conbajo_crpss.png",
       width = 10, height = 7, plot = p_low)




df = df_crpss_avg %>% 
  subset(version_sampling == "Mejor combinación Leave 1 out") %>% 
  subset(month_initialisation %in% c("1˚jul"))

df2 = subset(df_avgens,metric_name == "r2_avg") %>% 
  subset(version_sampling == "Mejor combinación Leave 1 out") %>% 
  subset(month_initialisation %in% c("1˚jul"))

crpss_map = plot_metric2(df,
                    metric = "value",
                    by_y = "catchment_code",
                    metric_name = "CRPSS",
                    all_x = F)+
  scale_fill_viridis_b(breaks = seq(0, 1, 0.25), limits = c(0, 1))+
  guides(fill = FALSE)+
  theme(plot.title = element_text(hjust = 0.5))

r2_map = plot_metric2(df2,
                                 metric = "metric_value",
                                 by_y = "catchment_code",
                                 metric_name = expression(R^2),
                                 all_x = F)+
  scale_fill_viridis_b(breaks = seq(0, 1, 0.25), limits = c(0, 1))+
  theme(plot.title = element_text(hjust = 0.5))

p_maps = grid.arrange(crpss_map,r2_map,ncol =2,widths = c(1,2))
ggsave(filename = "data_output/figuras/scores/crpss_r2_mapa_jul.png",
       width = 5, height = 7, plot = p_maps)
# ggplot(data = subset(df_crpss_avg, month_initialisation %in% c("1˚may","1˚jul","1˚sep") &
#                      version_sampling == "Mejor combinación Leave 1 out")) +
#   geom_point(aes(  x =  aridity_cr2met_1979_2010 ,
#                    y = value,
#                    col = month_initialisation))+
#   geom_text(
#     data =subset(df_crpss_avg,value<0.3 & month_initialisation %in% c("1˚may","1˚jul","1˚sep") &
#                    version_sampling == "Mejor combinación Leave 1 out"),
#     aes(label = gauge_name,
#         x =  aridity_cr2met_1979_2010 ,
#         y = value))+
#   facet_wrap(~month_initialisation) +
#   labs(x = "Indice aridez (P/PET) [mm/mm]",
#        y = "CRPSS c/r volumen promedio",
#        title = "CRPSS vs índice de aridez del modelo 'mejor combinación (AIC)' ",
#        col = "mes emisión")+
#   theme(legend.position = "bottom") +
#   guides(col = guide_legend(nrow = 2))


# ggsave(filename = "data_output/figuras/scores/scatter_crpss-promedio_aridez.png",
#        width = 10, height = 7, plot = p1)


p2 <- ggplot(data = subset(df_crpss_avg, version_sampling == "Mejor combinación Leave 1 out")) +
  geom_point(aes(  x =  hfd_mean ,
                   y = value,
                   col = month_initialisation))+
  facet_wrap(~month_initialisation)+
  labs(x = "Centroide del hidrograma (dia max q/365) [d/d]",
       y = "CRPSS c/r volumen promedio",
       title = "CRPSS vs centroide del hidrograma del modelo 'mejor combinación (AIC)' ",
       col = "mes emisión")+
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 2))

p2 = grid.arrange(p2,p_hfd,ncol =2,widths = c(3,1))

ggsave(filename = "data_output/figuras/scores/scatter_crpss-promedio_hfd.png",
       width = 10, height = 7, plot = p2)






p3 <- ggplot(data = subset(df_crpss_avg, version_sampling == "Mejor combinación Leave 1 out")) +
  geom_point(aes(  x =  p_mean_cr2met_1979_2010 ,
                   y = value,
                   col = month_initialisation))+
  facet_wrap(~month_initialisation)+
  labs(x = "Precipitación anual promedio 1979-2010 (CR2MET) [mm]",
       y = "CRPSS c/r volumen promedio",
       title = "CRPSS vs Precipitación media anual del modelo 'mejor combinación (AIC)' ",
       col = "mes emisión")+
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 2))

p3 = grid.arrange(p3,p_pmean,ncol =2,widths = c(3,1))

ggsave(filename = "data_output/figuras/scores/scatter_crpss-promedio_pmean.png",
       width = 10, height = 7, plot = p3)

p4 <- ggplot(data = subset(df_crpss_avg, version_sampling == "Mejor combinación Leave 1 out")) +
  geom_point(aes(  x =  runoff_ratio_cr2met_1979_2010 ,
                   y = value,
                   col = month_initialisation))+
  facet_wrap(~month_initialisation)+
  labs(x = "Coeficiente de escorrentía 1979-2010 (q/P) [mm/mm]",
       y = "CRPSS c/r volumen promedio",
       title = "CRPSS vs coeficiente de escorrentía del modelo 'mejor combinación (AIC)' ",
       col = "mes emisión")+
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 2))

p4 = grid.arrange(p4,p_runoffratio,ncol =2,widths = c(3,1))

ggsave(filename = "data_output/figuras/scores/scatter_crpss-promedio_rr.png",
       width = 10, height = 7, plot = p4)




#### respecto a la referencia




p5 <- ggplot(data = df_crpss) +
  geom_point(aes(  x =  aridity_cr2met_1979_2010 ,
                   y = crpss_storage,
                   col = month_initialisation))+
  facet_wrap(~month_initialisation)+
  labs(x = "Indice aridez (P/PET) [mm/mm]",
       y = "CRPSS c/r modelo referencia",
       title = "CRPSS vs índice de aridez del modelo 'mejor combinación (AIC)' ",
       col = "mes emisión")+
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 2))


p5 = grid.arrange(p5,p_aridez,ncol =2,widths = c(3,1))
ggsave(filename = "data_output/figuras/scores/scatter_crpss-referencia_aridez.png",
       width = 10, height = 7, plot = p5)

p6 <- ggplot(data = df_crpss) +
  geom_point(aes(  x =  hfd_mean ,
                   y = crpss_storage,
                   col = month_initialisation))+
  facet_wrap(~month_initialisation)+
  labs(x = "Centroide del hidrograma (dia max q/365) [d/d]",
       y = "CRPSS c/r modelo referencia",
       title = "CRPSS vs centroide del hidrograma del modelo 'mejor combinación (AIC)' ",
       col = "mes emisión")+
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 2))

p6 = grid.arrange(p6,p_hfd,ncol =2,widths = c(3,1))

ggsave(filename = "data_output/figuras/scores/scatter_crpss-referencia_hfd.png",
       width = 10, height = 7, plot = p6)

p7 <- ggplot(data = df_crpss) +
  geom_point(aes(  x =  p_mean_cr2met_1979_2010 ,
                   y = crpss_storage,
                   col = month_initialisation))+
  facet_wrap(~month_initialisation)+
  labs(x = "Precipitación anual promedio 1979-2010 (CR2MET) [mm]",
       y = "CRPSS c/r modelo referencia",
       title = "CRPSS vs Precipitación media anual del modelo 'mejor combinación (AIC)' ",
       col = "mes emisión")+
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 2))

p7 = grid.arrange(p7,p_pmean,ncol =2,widths = c(3,1))

ggsave(filename = "data_output/figuras/scores/scatter_crpss-referencia_pmean.png",
       width = 10, height = 7, plot = p7)

p8 <- ggplot(data = df_crpss) +
  geom_point(aes(  x =  runoff_ratio_cr2met_1979_2010 ,
                   y = crpss_storage,
                   col = month_initialisation))+
  facet_wrap(~month_initialisation)+
  labs(x = "Coeficiente de escorrentía 1979-2010 (q/P) [mm/mm]",
       y = "CRPSS c/r modelo referencia",
       title = "CRPSS vs coeficiente de escorrentía del modelo 'mejor combinación (AIC)' ",
       col = "mes emisión")+
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 2))

p8 = grid.arrange(p8,p_runoffratio,ncol =2,widths = c(3,1))
ggsave(filename = "data_output/figuras/scores/scatter_crpss-referencia_rr.png",
       width = 10, height = 7, plot = p8)


