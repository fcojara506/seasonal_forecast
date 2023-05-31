# Clear workspace
rm(list = ls())

# Load libraries
library(dplyr)
library(data.table)

# Define functions
join_x_info <- function(x, column = "scores_volume") {
  data <- x[[column]]
  info_x <- x[["info"]]
  predictor_list <- rbind(info_x[c("predictor_list")])
  
  months_es <-
    c("ene",
      "feb",
      "mar",
      "abr",
      "may",
      "jun",
      "jul",
      "ago",
      "sep",
      "oct",
      "nov",
      "dic")
  
  df <- data.table(data) %>%
    cbind(predictor_list) %>%
    mutate(month_initialisation = paste0("1˚", months_es[(month(info_x$datetime_initialisation))])) %>%
    mutate(catchment_code = info_x$catchment_code)
  
  return(df)
}

sort_months <- function(scores) {
  months_wy <-
    c("abr",
      "may",
      "jun",
      "jul",
      "ago",
      "sep",
      "oct",
      "nov",
      "dic",
      "ene",
      "feb",
      "mar")
  scores$month_initialisation <-
    factor(scores$month_initialisation, levels = paste0("1˚", months_wy))
  
  return(scores)
}

read_and_process_scores <- function(file_path, column) {
  scores <- readRDS(file = file_path) %>%
    lapply(
      FUN = function(x)
        join_x_info(x, column = column)
    ) %>%
    rbindlist() %>%
    sort_months() %>%
    data.table() %>%
    select(-"predictor_list") %>% 
    #select(-c("predictor_list", "mae_obs")) %>%
    melt.data.table(
      id.vars = c("catchment_code", "month_initialisation"),
      variable.name = "metric_name",
      value.name = "metric_value"
    )
  
  return(scores)
}




################# read files
summary_scores_data <- function(scores_data) {
  summary_data <- scores_data %>%
    group_by(month_initialisation, metric_name) %>%
    summarise(
      mean_value = mean(metric_value, na.rm = TRUE),
      median_value = median(metric_value, na.rm = TRUE),
      min_value = min(metric_value, na.rm = TRUE),
      max_value = max(metric_value, na.rm = TRUE),
      .groups = 'drop'  # automatically drop group_by after computation
    ) %>%
    data.table()
  return(summary_data)
}

lista_path_metricas =
  list(
    "data_output/metricas/RDS/scores_best_20230431_mm.RDS",
    "data_output/metricas/RDS/scores_reference_20230431_mm.RDS",
    "data_output/metricas/RDS/scores_best_20230431.RDS",
    "data_output/metricas/RDS/scores_reference_20230431.RDS"
  )

for (file in lista_path_metricas) {
  filename = basename(path = file)
  filename_csv <- sub("\\.RDS$", ".csv", filename)
  filename_csv <- sub("scores", "", filename_csv)
  filename_csv <- sub("reference", "referencia", filename_csv)
  filename_csv <- sub("best", "final", filename_csv)
  
  ## metricas de VOLUMEN
  scores_vol_cuencas = read_and_process_scores(file_path = file,
                                               column = "scores_volume") %>%
    data.table()
  #por cuenca
  write.csv(
    x = scores_vol_cuencas,
    file = paste0(
      "data_output/metricas/csv/por_cuenca/metricas_vol_cuencas",
      filename_csv
    ),
    row.names = F
  )
  # resumen
  write.csv(
    x =  summary_scores_data(scores_vol_cuencas),
    file = paste0(
      "data_output/metricas/csv/resumen_agrupadas/resumen_metricas_vol",
      filename_csv
    ),
    row.names = F
  )
  
  ## metricas de CAUDAL
  scores_caudal_cuencas = read_and_process_scores(file_path = file,
                                               column = "scores_flow") %>%
    data.table()
  #por cuenca
  write.csv(
    x = scores_caudal_cuencas,
    file = paste0(
      "data_output/metricas/csv/por_cuenca/metricas_caudal_cuencas",
      filename_csv
    ),
    row.names = F
  )
  # resumen
  write.csv(
    x =  summary_scores_data(scores_caudal_cuencas),
    file = paste0(
      "data_output/metricas/csv/resumen_agrupadas/resumen_metricas_caudal",
      filename_csv
    ),
    row.names = F
  )
  
}

# # promedio
# promedio =
#   dcast.data.table(
#     summary_data,
#     metric_name ~ month_initialisation,
#     value.var = "mean_value",
#     na.rm = TRUE
#   )
#
# mediana =
#   dcast.data.table(
#     summary_data,
#     metric_name ~ month_initialisation,
#     value.var = "median_value",
#     na.rm = TRUE
#   )
# minimo =
#   dcast.data.table(
#     summary_data,
#     metric_name ~ month_initialisation,
#     value.var = "min_value",
#     na.rm = TRUE
#   )
#
# maximo =
#   dcast.data.table(
#     summary_data,
#     metric_name ~ month_initialisation,
#     value.var = "max_value",
#     na.rm = TRUE
#   )
