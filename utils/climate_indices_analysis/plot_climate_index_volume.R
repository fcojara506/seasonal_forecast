rm(list = ls())
source("base/Charts.R")


correlation_plot = list(
filename_input  = "data_output/scores/RDS/correlations_climates_indices_vol.RDS",
metric_variable = "correlation",
filename_export = "correlation_vol_climate_indices_pheatmap",
legend_title = "Correlación (Spearman)",
plot_absolute = T
)

p = do.call(plot_pheatmap_EDA,correlation_plot)



CRPSS_plot = list(
    filename_input  = "data_output/scores/RDS/model_results_2023-01-20.RDS",
  metric_variable = "crpss_climatology",
  filename_export = "CRPSS_vol_climate_indices_pheatmap",
  legend_title = "CRPSS (climatología)",
  plot_absolute = F
)
do.call(plot_pheatmap_EDA,CRPSS_plot)

