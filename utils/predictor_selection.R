# clear environment and set working directory
rm(list = ls())
setwd(rprojroot::find_rstudio_root_file())

# load required packages
library(lubridate)
library(dplyr)
library(caret)
library(locfit)

# source required scripts
source("base/Preprocess_data.R")
source("base/Regression_model.R")

# set catchment code
catchment_code <- "4703002"

# define predictors
a <- grid_pred(c("SOI", "PDO", "MEIv2", "ONI", "ESPI"), -1, "mean")
c <- grid_pred(c("STORAGE"), 1, "last")
predictors <- c(a, c)

# preprocess input data
data_input <- preprocess_data(
  datetime_initialisation = as.Date("2022-11-01"),
  forecast_mode = "cv",
  catchment_code = catchment_code,
  predictor_list = predictors
)
predictor_list <- data_input$info$predictor_list[[1]]

# set regression model parameters
X_train <- data_input$X_train
y_train <- data_input$y_train$volume
resampling_method <- "none"
preProcess <- c("center", "scale")
method <- "lm"
trcontrol <- trainControl(method = resampling_method, savePredictions = "all")

# remove correlated predictors
cor_matrix <- cor(X_train)
high_cor <- findCorrelation(cor_matrix, cutoff = 0.8, verbose = FALSE, exact = TRUE)
X_train_uncorr <- X_train[, -high_cor]
predictors_uncorrelated <- predictor_list[-high_cor]

# generate all possible combinations of predictors
predictors_comb <- unlist(lapply(seq_along(predictors_uncorrelated), function(i)
  combn(predictors_uncorrelated, i, simplify = FALSE)), recursive = FALSE)

# set months of initialisation
months_initialisation <- 9

# train regression model for each combination of predictors and month of initialisation
models <- list()
for (month_initialisation in months_initialisation) {
  for (predictor in predictors_comb) {
    # preprocess input data
    data_input <- preprocess_data(
      datetime_initialisation = lubridate::make_date(2022, month_initialisation, 1),
      forecast_mode = "cv",
      catchment_code = catchment_code,
      predictor_list = predictor
    )
    # train regression model
    regression_model <- train(
      data_input$X_train,
      data_input$y_train$volume,
      metric = "RMSE",
      trControl = trcontrol,
      method = method,
      preProcess = preProcess
    )
    # scores
    mlrmod  = regression_model$finalModel
    pr        = resid(mlrmod)/(1 - lm.influence(mlrmod)$hat)
    
    metrics = 
    list(
    gvc = unlist(gcv(mlrmod)[[4]]),
    bic = BIC(mlrmod),
    press = sum(pr^2),
    aic = AIC(mlrmod),
    rmse = RMSE(pred = mlrmod$fitted.values,obs = data_input$y_train$volume )
    )
    
    # calculate variable importance if there are multiple predictors
    imp_var <- if (length(predictor) > 1) relaimpo::calc.relimp(mlrmod, rela = TRUE)$lmg else list(predictor = 0)
    
    # save results
    models[[length(models) + 1]] <- list(
      info = data_input$info,
      reg = regression_model$finalModel,
      imp_var = convert_items_to_lists(imp_var),
      metrics = metrics
    )
  }
}

a = models %>% purrr::transpose()
b = rbindlist(a$metrics)
c = rbindlist(a$info)
d = rbindlist(a$imp_var,fill = T)
