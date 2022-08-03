source("Preprocess_data.R")
source("Regression_model.R")

data = preprocess_data(
  catchment_code = '5410002',
  month_initialisation = "oct",
  window_strategy = "dynamic",
  predictor_list = c("pr_sum_-1months","tem_mean_3months"),
  wy_holdout = 2016
)

data_fore = forecast_vol_ensemble(
  data = data,
  method = "lm",
  n_members = 1000)

scores = get_scores(
  data_fore = data_fore,
  data = data)

subplot_boxplot_chronological_median_order(data,data_fore,subplot = T,export = T,plot=F)
