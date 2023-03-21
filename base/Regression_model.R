# rmse_LOO is used to calculate the RMSE in Leave One Out cross-validation
# train_regression_model is used to train the linear regression model
# cross_validation is used to perform cross-validation on the trained model
# make_predictions is used to make predictions on the test data
# forecast_vol_determinist is the main function that calls the above functions and returns the results of the deterministic forecast
# ensemble_generator is used to generate an ensemble of predictions
# ensemble_post is used to post-process the ensemble of predictions
# forecast_vol_ensemble is the main function that calls the above functions and returns the results of the ensemble forecast

library(caret)

set.seed(10)
############## RMSE IN LEAVE.ONE.OUT #######################
# Function to calculate the RMSE in Leave One Out cross-validation
rmse_LOO <- function(simulated_values, observed_values) {
  require(caret)
  
  # Check that the lengths of simulated and observed values match
  if (length(simulated_values) != length(observed_values)) {
    stop("Size of simulated values is NOT equal to observed values in RMSE")
  }
  # Initialize a matrix to store the RMSE values for each iteration
  rmse_matrix <- matrix(nrow = length(simulated_values))
  # Loop through each simulated value and calculate the RMSE 
  for (iterator in seq_along(simulated_values)) {
    rmse_matrix[iterator] <- caret::RMSE(pred = simulated_values[-iterator],
                                            obs = observed_values[-iterator])
  }
  return(rmse_matrix)
}
############## REGRESSION
# Function for training the linear regression model
train_regression_model <- function(X_train, y_train,method = "lm",preProcess = c("center", "scale"),resampling_method = "LOOCV",metric = "RMSE",...) {
  library(caret)
  #Train a regression model using the provided data and method

  train(
    X_train,
    y_train,
    metric = metric,
    trControl = trainControl(method = resampling_method,
                             savePredictions = "all"),
    method = method,
    preProcess = preProcess,
    ...
  )
}

# Function for performing cross validation
cross_validation <- function(regression_model) {
  #Get the best model predictions and merge them with the actual values 
  y_best_model_pred = merge(
    regression_model$bestTune,
    regression_model$pred) %>%
    arrange(rowIndex)
  #Get the cross-validated simulated values
  y_cv                <- y_best_model_pred$pred
  obs_cv              <- y_best_model_pred$obs

  # Calculate the RMSE in cross-validation
  rmse_cv              <- rmse_LOO(sim = y_cv,obs = obs_cv)
  rmse_model           <- caret::RMSE(pred = y_cv, obs = obs_cv)
  
  return(list(y_cv = y_cv, rmse_cv = rmse_cv, rmse_model = rmse_model))
}

# Function for making predictions on test data
make_predictions <- function(regression_model, X_test) {
  # Make predictions using the trained model and test data
  y_fore = NULL
  if (!is.null(X_test)) {
    y_fore <- predict(regression_model, newdata = X_test)
  } 
  return(y_fore)
}



# Main function that calls the above functions
forecast_vol_determinist <- function(X_train, y_train, X_test,function_y=NULL,method='lm', preProcess = c("center", "scale"), forecast_mode = "both", ...) {
  # check mode
  if(!(forecast_mode %in% c("cv","prediction","both"))) stop("Invalid mode provided, please provide one of these cv,prediction,both.")
  #Train the regression model using the provided data and method
  regression_model <- train_regression_model(X_train, y_train, method, preProcess,tuneLength = 10,...)
  
  if (!is.null(function_y)) {
    regression_model$pred$obs = expo(regression_model$pred$obs)
    regression_model$pred$pred = expo(regression_model$pred$pred)
  }
  
  pred_obs_model <- merge(regression_model$bestTune,regression_model$pred)
  
  # error from regression model
  rmse_model <- caret::RMSE(pred = pred_obs_model$pred ,obs = pred_obs_model$obs)
  #Initialise variables
  y_cv <- NULL
  rmse_cv <- NULL
  y_fore <- NULL
  
  if (forecast_mode == "both" || forecast_mode == "cv") {
    # Perform cross validation and get the results
    cv_results <- cross_validation(regression_model)
    y_cv <- cv_results$y_cv
    rmse_cv <- cv_results$rmse_cv
  }
  
  if (forecast_mode == "both" || forecast_mode == "prediction") {
    # Make predictions on the test data
    y_fore <- make_predictions(regression_model, X_test)
    
  }
  
  
  return(list(y_cv = y_cv,
              y_fore = y_fore,
              rmse_cv = rmse_cv,
              rmse_model = rmse_model,
              regression_model = regression_model))
}

# Function to generate an ensemble of predictions
ensemble_generator <- function(y,rmse,n_members=1000){
  
  library(truncnorm)
  # Check that the lengths of observed values and rmse match
  if (length(y) != length(rmse)) {
    stop("Size of observed values is NOT equal to rmse in ensemble generation")
  }
  # Initialize a matrix to store the ensemble of predictions
  ensemble_vol           <- matrix(nrow=n_members, ncol=length(rmse))
  # Loop through each year and generate random perturbations based on the RMSE
  for (i_year in seq_along(rmse)) {
    # Get the center and variation values for the current year
    center = y[i_year]
    variation = rmse[i_year]
    # volume ensemble = center +- variation
    # Generate random perturbations using truncated normal distribution
    ensemble_vol[,i_year] <-  rtruncnorm(n = n_members,
                                         mean = center,
                                         sd = variation,
                                         a = 0,
                                         b = Inf) %>%
      matrix(n_members,1)
  }  

  return(ensemble_vol)
}

  ensemble_cv <- function(vol_deterministic, data_input, n_members) {
    y_ens_cv = NULL
    # Generate ensemble for cross-validation mode
    y_ens_cv = ensemble_generator(y = vol_deterministic$y_cv,
                                  rmse = vol_deterministic$rmse_cv,
                                  n_members = n_members)
    
    # Set column names for cross-validation mode
    colnames(y_ens_cv) = data_input$wy_train
    
    return(list(y_ens_cv = y_ens_cv))
  }
  
  ensemble_test <- function(vol_deterministic, data_input, n_members) {
    # Generate ensemble for test mode if test data is available
    y_ens_fore = NULL
    if (! is.null(vol_deterministic$y_fore)) {
      y_ens_fore = ensemble_generator(y = vol_deterministic$y_fore,
                                      rmse = vol_deterministic$rmse_model,
                                      n_members = n_members)
      
    }
    
    # Set column names for test mode
    colnames(y_ens_fore) = data_input$info$wy_holdout
    
    return(list(y_ens_fore = y_ens_fore))
  }
  
  expo <- function(x) {
    y = x
    if (!is.null(x)) {
      y = exp(x)
    }
    return(y)
  }
  
  ensemble_cv_and_test <- function(vol_deterministic, data_input, n_members, forecast_mode = c("both","cv", "prediction")) {
    # check mode
    if(!(forecast_mode %in% c("cv","prediction","both"))) stop("Invalid mode provided, please choose one of these: 'cv', 'prediction', 'both'")
    
    # if (!is.null(data_input$info$y_transform$function_y)) {
    #   vol_deterministic$y_cv = expo(vol_deterministic$y_cv)
    #   vol_deterministic$y_fore = expo(vol_deterministic$y_fore)
    #   vol_deterministic$rmse_cv = expo(vol_deterministic$rmse_cv)
    #   vol_deterministic$rmse_model = expo(vol_deterministic$rmse_model)
    # }
    # 
    # Initialise ensemble variables
    y_ens_cv = NULL
    y_ens_fore = NULL
    
    if (forecast_mode == "both" || forecast_mode == "cv") {
      # Perform cross validation and get the results
      y_ens_cv = ensemble_cv(vol_deterministic, data_input, n_members)
    }
    if (forecast_mode == "both" || forecast_mode == "prediction") {
      # Make predictions on the test data
      y_ens_fore = ensemble_test(vol_deterministic, data_input, n_members)
    }
    
    return(c(y_ens_cv,y_ens_fore,vol_deterministic))
  }
  

forecast_vol_ensemble <- function(data_input,
                                  n_members=1000,
                                  method='lm',
                                  preProcess = c("center", "scale"),
                                  forecast_mode = data_input$info$forecast_mode
                                  ){
  model_info = as.list(environment())
  model_info$data_input <- NULL
  model_info$forecast_mode <- NULL
  model_info = lapply(model_info, function(x) if (length(x) > 1) list(x) else x)
  
 
  
  
  
  # Train and predict using regression model
    vol_deterministic =
      forecast_vol_determinist(
        X_train = data_input$X_train,
        y_train = data_input$y_train$volume,
        X_test = data_input$X_test,
        function_y = data_input$info$y_transform$function_y,
        method = method,
        preProcess = preProcess,
        forecast_mode = forecast_mode
      )
    


    #https://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html
    
    # Generate ensemble forecast
    y_forecast = ensemble_cv_and_test(vol_deterministic, data_input, n_members,forecast_mode)

    return(append(y_forecast,list(model_info = model_info)))
}
