# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Documents/GitHub/seasonal_forecast")

# Load required packages
library(lubridate)
library(dplyr)
library(caret)


# Source required scripts
source("base/Preprocess_data.R")
source("base/Regression_model.R")

# ----------------------
# Functions
# ----------------------

# Function for removing correlated predictors
remove_correlated_predictors <- function(X_train, predictor_list, cutoff = 0.8) {
  cor_matrix <- cor(X_train)
  high_cor <- findCorrelation(cor_matrix, cutoff = cutoff, verbose = FALSE)
  
  if (rlang::is_empty(high_cor)) {
    return(predictor_list)
  } else {
    return(predictor_list[-high_cor])
  }
}


# Function for calculating model metrics
calculate_model_metrics <- function(regression_model) {
  library(locfit)
  mlrmod <- regression_model$finalModel
  pr <- resid(mlrmod) / (1 - lm.influence(mlrmod)$hat)
  
  metrics <- list(
    gcv = unlist(gcv(mlrmod,maxk=1e6)[[4]]),
    bic = BIC(mlrmod),
    press = sum(pr^2),
    aic = AIC(mlrmod),
    rmse = RMSE(pred = mlrmod$fitted.values, obs = regression_model$pred$obs)
  )
  
  return(metrics)
}

# Function for saving model results
save_model_results <- function(df_info,
                               predictor,
                               regression_model,
                               imp_var,
                               metrics) {
  result <- list(
    df_info = df_info,
    predictor = convert_items_to_lists(predictor),
    reg_model = regression_model,
    imp_var = (imp_var),
    metrics = (metrics)
  )
  
  
  return((result))
}

# ------------------------------
# Main code for training models
# ------------------------------

# Set catchment code and months of initialization
catchment_code <- "4703002"
months_initialisation <- 5:9

# Define predictors
a <- grid_pred(c("SOI", "PDO", "ONI","NINO1.2"), 1, "mean")
b <- grid_pred(c("STORAGE"), 1, "last")
predictors <- c(a, b)

library(foreach)
library(doSNOW)
cl <- makeCluster(parallel::detectCores()-1L)
registerDoSNOW(cl)


# Train regression models for each combination of predictors and month of initialization
models <-
foreach(month_initialisation=months_initialisation,.combine = "c") %dopar% { 
  library(foreach)
  # Source required scripts
  source("base/Preprocess_data.R")
  source("base/Regression_model.R")
  # Preprocess input data
  data_input <- preprocess_data(
    datetime_initialisation = lubridate::make_date(2022, month_initialisation, 1),
    forecast_mode = "cv",
    catchment_code = catchment_code,
    predictor_list = predictors
  )
  predictor_list <- data_input$info$predictor_list[[1]]
  
  # Remove correlated predictors
  predictors_uncorrelated <- remove_correlated_predictors(data_input$X_train, predictor_list)
  
  # Generate all possible combinations of predictors
  predictors_comb <- unlist(lapply(seq_along(predictors_uncorrelated), function(i)
    combn(predictors_uncorrelated, i, simplify = FALSE)), recursive = FALSE)
  
  #Iterate through each predictor combination
  #for (predictor in predictors_comb) {
    foreach(predictor=predictors_comb) %do% {
    #print(paste(length(models) + 1, "/", length(predictors_comb)))
    #print(paste0("predictor: ", paste0(predictor, collapse = "+"), " month: ", month_initialisation))
    # Preprocess input data
    data_input <- preprocess_data(
      datetime_initialisation = lubridate::make_date(2022, month_initialisation, 1),
      forecast_mode = "cv",
      catchment_code = catchment_code,
      predictor_list = predictor
    )
    
    # Train regression model
    regression_model <- train_regression_model(data_input$X_train,
                                               data_input$y_train$volume,
                                               method = "lm"
                                                )
    
    # Calculate model metrics
    metrics <- calculate_model_metrics(regression_model)

    # Calculate variable importance if there are multiple predictors
    imp_var <- if (length(predictor) > 1) relaimpo::calc.relimp(regression_model$finalModel, rela = TRUE)$lmg else list(x = 0)
  
    
    # Save results
    #models[[length(models) + 1]] 
    model <- save_model_results(data.frame(month_initialisation,catchment_code),
                                                       list(predictor = data_input$info$predictor_list),
                                                       regression_model,
                                                       data.frame(t(imp_var)),
                                                       metrics)
  }
}

stopCluster(cl)
# ------------------------------
#   Post-process the results
# ------------------------------
select_best_models <- function(models) {
  
model_list <- purrr::transpose(models)
predictor_list =  rbindlist(model_list$predictor)



best <- cbind( rbindlist(model_list$df_info),
            rbindlist(model_list$metrics),
            predictor_list,
            select(rbindlist(model_list$imp_var,fill = T),-x),
            model = model_list$reg_model
            ) %>%
  group_by(month_initialisation,catchment_code) %>% 
  slice(which.min(aic))

return(list(model = best,
            unique_predictors = unique(unlist(predictor_list))))
}

best = select_best_models(models)

