

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


# Main script
files <- c("data_output/scores/RDS/scores_best_20230425.RDS")


scores_data <- process_files(files)[[1]] %>% data.table()
scores_data %>% colnames
scores_data %>% colnames()
library(dplyr)

summary_data <- scores_data %>%
  group_by(month_initialisation, metric_name) %>%
  summarise(
    mean_value = mean(metric_value, na.rm = TRUE),
    median_value = median(metric_value,na.rm = TRUE),
    min_value = min(metric_value, na.rm = TRUE),
    max_value = max(metric_value, na.rm = TRUE),
    .groups = 'drop'  # automatically drop group_by after computation
  ) %>% 
  data.table()


# promedio
promedio =
  dcast.data.table(
    summary_data,
    metric_name ~ month_initialisation,
    value.var = "mean_value",
    na.rm = TRUE
  )

mediana =
  dcast.data.table(
    summary_data,
    metric_name ~ month_initialisation,
    value.var = "median_value",
    na.rm = TRUE
  )
minimo =
  dcast.data.table(
    summary_data,
    metric_name ~ month_initialisation,
    value.var = "min_value",
    na.rm = TRUE
  )

maximo =
  dcast.data.table(
    summary_data,
    metric_name ~ month_initialisation,
    value.var = "max_value",
    na.rm = TRUE
  )

  
  
