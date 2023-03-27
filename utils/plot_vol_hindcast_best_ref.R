rm(list = ls())
source("base/Preprocess_data.R")
source("base/Regression_model.R")
source("base/Charts.R")

forecast_mode = "cv"

catchment_code = 7321002

month_initialisation <- 7

data_best_models = readRDS(file = paste0(
  "data_output/mejores_modelos_cuenca_mes/",
  catchment_code,
  "_may-sep.RDS"
))

best_combination = data_best_models$best_combination
best_combination = best_combination[best_combination$month_initialisation == month_initialisation, ]


datetime_emission = lubridate::make_date(2022, month_initialisation)
if (month_initialisation < 4) {
  datetime_emission = datetime_emission %m+% months(12)
}

## best combination of predictors
data_input_best <- preprocess_data(
  datetime_initialisation = datetime_emission,
  forecast_mode = forecast_mode,
  catchment_code = catchment_code,
  predictor_list = unlist(best_combination$predictors),
  save_raw = T
)

# ensemble volume forecast
data_fore_best = forecast_vol_ensemble(data_input = data_input_best,
                                       forecast_mode = forecast_mode)

data_input_ref <- preprocess_data(
  datetime_initialisation = datetime_emission,
  forecast_mode = forecast_mode,
  catchment_code = catchment_code,
  predictor_list = "STORAGE_last_1months",
  save_raw = T
)

# ensemble volume forecast
data_fore_ref = forecast_vol_ensemble(data_input = data_input_ref,
                                      forecast_mode = forecast_mode)






# library(ggplot2)
# library(grid)
# library(ggtext)
# 
# data_plot_best = data_plot_backtest_volume(data_input = data_input_best, data_fore = data_fore_best)
# data_plot_ref = data_plot_backtest_volume(data_input = data_input_ref, data_fore = data_fore_ref)
# 
# 
# y_ens = rbind(
#   mutate(data_plot_best$y_ens, version = "best"),
#   mutate(data_plot_ref$y_ens, version = "ref")
# ) %>%
#   data.frame()
# 
# 
# y_ens$wy_simple = as.factor(as.numeric(y_ens$wy_simple))
# df_train = data_plot_best$df_train
# 
# quantiles_obs = data_plot_best$quantiles_obs
# quantiles_text = quantiles_obs %>% rownames_to_column(var = "quantiles")
# 
# x_labels = unique(y_ens$wy_simple)
# x_limits = length(x_labels)
# 
# 
# # plot ensembles
# ggplot() +
#   geom_boxplot(
#     data = y_ens,
#     aes(x = wy_simple,
#         y = volume,
#         fill = version),
#     lwd = 0.1,
#     # width = ,
#     outlier.size = 0.1
#   ) +
#   # # change labels
#   labs(y =  glue("Volumen ({data_input_best$info$water_units$y})"),
#        x = "Año hidrológico (orden cronológico)") +
#   theme(axis.text.x = element_text(
#     angle = 90,
#     vjust = 0.5,
#     hjust = 1
#   )) +
#   geom_point(
#     data = df_train,
#     aes(x = wy_simple,
#         y = obs,
#         col = " "),
#     shape = 4,
#     size  = 2
#   ) +
#   scale_color_manual(values = c(" " = "black"),
#                      name = "Caudal estación Fluviométrica") +
#   # add observed quantiles
#   theme(plot.margin = unit(c(1, 5, 1, 0.5), "lines")) +
#   geom_text(
#     data = quantiles_text,
#     aes(
#       x = x_limits + 2,
#       y = quantiles_obs,
#       label = glue(
#         "{quantiles} ({round(quantiles_obs,1)} {data_input_best$info$water_units$y})"
#       )
#     ),
#     vjust = 0.4,
#     hjust = 0,
#     size = 2
#   ) +
#   coord_cartesian(xlim = c(0, x_limits + 1), clip = "off") +
#   geom_segment(
#     data = quantiles_text,
#     aes(
#       x = 1,
#       xend = x_limits,
#       y = quantiles_obs,
#       yend = quantiles_obs
#     ),
#     linetype = "dashed"
#   ) +
#   theme(legend.position = "bottom",
#         legend.spacing.x = unit(0, 'cm')) +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(
#     title =  glue(
#       "Pronóstico retrospectivo de volumen {data_input_best$time_horizon$volume_span_text}"
#     ),
#     subtitle = data_input_best$raw_data$attributes_catchment$gauge_name,
#     caption  = glue(
#       "Emisión {format(data_input_best$time_horizon$datetime_initialisation, '1˚ %b')}"
#     )
#   )
