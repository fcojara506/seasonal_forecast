rm(list = ls())
source("base/Preprocess_data.R")
source("base/Regression_model.R")

forecast_mode = "both"
catchment_code = 5410002
month_initialisation <- 9

data_input <- preprocess_data(
  datetime_initialisation = lubridate::make_date(1999, month_initialisation),
  forecast_mode = forecast_mode,
  catchment_code = catchment_code,
  predictor_list = c("STORAGE_mean_1months","SOI_mean_1months"),
  save_raw = T
)

data_fore = forecast_vol_ensemble(data_input = data_input,
                                  resampling_method = "cv")
# Train and predict using regression model


X_train = data_input$X_train
y_train = data_input$y_train$volume

method = "lm"
preProcess = c("center", "scale")
resampling_method = "LOOCV"
metric = "RMSE"


set.seed(42)

#
# library(doParallel)
# cl <- makeCluster(5)
# registerDoParallel(cl)

## Leave one out cross-validation
reg_loocv = 
  train(
  X_train,
  y_train,
  metric = metric,
  trControl = trainControl(method="LOOCV",savePredictions = "all",number = 13),
  method = method,
  preProcess = preProcess
)
RMSE(pred = reg_loocv$pred$pred,obs = reg_loocv$pred$obs)

pred_obs_model <- reg_loocv$pred %>% rownames_to_column(var = "wy")
cv_results <- cross_validation(reg_loocv)
y_ens_cv = ensemble_generator(cv_results$y_cv,
                              rmse = cv_results$rmse_cv,
                              n_members = 100)
colnames(y_ens_cv) = data_input$wy_train
a = y_ens_cv %>% data.table() %>%
  melt.data.table(variable.name  = c("wy")) %>% 
  merge(pred_obs_model)

plot(exp(a$obs),exp(a$value))



reg_kfold = 
  train(
    X_train,
    y_train,
    metric = metric,
    trControl = trainControl(method="cv",
                             number=13,
                             savePredictions = "all"),
    method = method,
    preProcess = preProcess
  )
a1 = reg_kfold$pred %>% arrange(rowIndex)
a2 = reg_loocv$pred %>% arrange(rowIndex)
RMSE(pred = reg_kfold$pred$pred,obs = reg_kfold$pred$obs)

# reg_repcv = 
#   train(
#     X_train,
#     y_train,
#     metric = metric,
#     trControl = trainControl(method="repeatedcv",
#                              number=2,
#                              repeats = 100,
#                              savePredictions = "all",
#                              allowParallel = T
#                              ),
#     method = method,
#     preProcess = preProcess
#   )
# 
# 
# plot((reg_repcv$pred$obs),(reg_repcv$pred$pred))
# MAE(pred = reg_repcv$pred$pred,obs = reg_repcv$pred$obs )
# sd(reg_repcv$pred$pred)

