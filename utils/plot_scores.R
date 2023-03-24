rm(list = ls())

library(dplyr)
library(data.table)

join_x_info <- function(x) {
  data = x[["scores_volume"]]
  info_x = x[["info"]]
  
  predictor_list = rbind(info_x[c("predictor_list")])
  #sort month names
  months_es <- c("ene", "feb", "mar","abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic")
  df = data.table(data) %>% 
    cbind(predictor_list) %>% 
    mutate(month_initialisation = paste0("1˚",months_es[(month(info_x$datetime_initialisation))])) %>% 
    mutate(catchment_code = info_x$catchment_code)
  
  return(df)
  
}

sort_months <- function(scores) {
  #sort month names
  months_wy <- c("abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic", "ene", "feb", "mar")
  scores$month_initialisation = factor(scores$month_initialisation, levels = paste0("1˚",months_wy) )
  
  return(scores)
}

scores = readRDS(file = "data_output/scores/RDS/scores_20230324.RDS") %>% 
  lapply(join_x_info) %>% 
  rbindlist()%>% 
  sort_months()

scores_ref = readRDS(file = "data_output/scores/RDS/scores_reference_20230324.RDS") %>% 
  lapply(join_x_info) %>% 
  rbindlist() %>% 
  sort_months() 

df_ref = scores_ref %>%
  data.table() %>%
  select(-c("predictor_list","mae_obs")) %>%
  melt.data.table(id.vars = c("catchment_code","month_initialisation"),
                  variable.name = "metric_name",value.name = "metric_value")

df = scores %>%
  data.table() %>%
  select(-c("predictor_list","mae_obs")) %>%
  melt.data.table(id.vars = c("catchment_code","month_initialisation"),
       variable.name = "metric_name",value.name = "metric_value")

df_comb = merge.data.table(df,df_ref,
                by=c("catchment_code","month_initialisation","metric_name"),
                suffixes = c("_best","_ref")  )
###############


attributes_catchments <- fread("data_input/attributes/attributes_49catchments_ChileCentral.csv" )



df_crpss = df_comb %>%
  subset(metric_name == "crps_ens") %>%
  mutate(crpss_storage = 1 - (metric_value_best /metric_value_ref)) %>% 
  select(c("catchment_code","month_initialisation","crpss_storage")) %>% 
  merge.data.table(attributes_catchments,by.x = "catchment_code",by.y = "cod_cuenca")

df_avgens = df_comb %>%
  subset(!(metric_name %in% c("crps_ens","crpss_climatology"))) %>%
  dplyr::rename("ref" = "metric_value_ref") %>% 
  dplyr::rename("best" = "metric_value_best") %>% 
  melt.data.table(id.vars =c("catchment_code","month_initialisation","metric_name"),
                  variable.name = "version",
                  value.name = "metric_value")%>% 
  merge.data.table(attributes_catchments,by.x = "catchment_code",by.y = "cod_cuenca")


library(ggplot2)

# plot of deterministic metrics
ggplot(data = df_avgens)+
  geom_boxplot(aes(x = month_initialisation,y = metric_value,col = version))+
  facet_wrap(~metric_name,scales = "free_y")


ggplot(data = df_avgens,aes(x = metric_value,y = gauge_lat, col = month_initialisation))+
  geom_point()+
  facet_wrap(~metric_name,scales = "free_x")

#plot of CRPSS respect to the storage (initial condition)
ggplot(data = df_crpss)+
  geom_boxplot(aes(x = month_initialisation,
                   y = crpss_storage))

##
ggplot(data = df_crpss,aes(x = crpss_storage,y = gauge_lat, col = month_initialisation))+
  geom_point()

ggplot(data = df_avgens,aes(x = metric_value,fill=month_initialisation))+
  geom_histogram(aes(y=after_stat(count)/sum(after_stat(count))))+
  facet_wrap(~metric_name,scales = "free")

