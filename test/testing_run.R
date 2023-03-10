rm(list = ls())
source("base/Preprocess_data.R")
#input data from preprocess
data_input <- preprocess_data(datetime_initialisation = '2020-06-01',
                              forecast_mode = "both",
                              catchment_code = "5410002",
                              predictor_list = c("pr_sum_-1months"))

source("base/Regression_model.R")
# Train and predict using regression model


f1 <- function(reg_method = "lm",boot_times = 500) {
  
regression_model <- train_regression_model(data_input$X_train,
                                           data_input$y_train$volume,
                                           method = reg_method,
                                           preProcess = c("center", "scale"),
                                           resampling_method = "LOOCV"
                                           )


bootSamples <- boot(data_input, function(data, idx) {
  tune = regression_model$bestTune
  bootstrapMod <- train_regression_model(data_input$X_train[idx, ],
                                         data_input$y_train[idx, ],
                                         method = reg_method,
                                         preProcess = c("center", "scale"),
                                         tuneGrid = tune,
                                         resampling_method = "none"
  )
  
  
  as.vector(coef(bootstrapMod$finalModel, tune[,ncol(tune)] ))

}, boot_times)

return(list(rm=regression_model,boot= bootSamples))
}

  xx = mutate(data_input$X_train, id = row_number())
  xx[idx,]

yy = data_input$y_train[idx, ]


m1 = f1("lm")
m2 = f1("glmnet")
m3 = f1("knn")
make_predictions(m1$rm,X_test = data_input$X_test)
make_predictions(m2$rm,X_test = data_input$X_test)
make_predictions(m3$rm,X_test = data_input$X_test)
data_input$y_test

m1$boot
m2$boot
m3$boot

# r1 = vol_deterministic2$regression_model
# plot(r1)
# 
# y_best_model_pred = merge(
#   r1$bestTune,
#   r1$pred) %>%
#   arrange(rowIndex)


# 
# 
# # ensemble volume forecast
# data_fore = forecast_vol_ensemble(data_input = data_input,
#                                    method = "lm")
# 
# #ensemble flow forecast
# source("base/Knn_model.R")
# q_fore = run_q_forecast(data_input = data_input,
#                          data_fore = data_fore)
# #export results
# source("base/Export_data.R")
# output = export_data(data_input = data_input,
#                      data_fore = data_fore,
#                      q_fore = q_fore,
#                      export = "scores")
