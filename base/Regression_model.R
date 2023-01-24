# rmse_LOO is used to calculate the RMSE in Leave One Out cross-validation
# train_regression_model is used to train the linear regression model
# cross_validation is used to perform cross-validation on the trained model
# make_predictions is used to make predictions on the test data
# forecast_vol_determinist is the main function that calls the above functions and returns the results of the deterministic forecast
# ensemble_generator is used to generate an ensemble of predictions
# ensemble_post is used to post-process the ensemble of predictions
# forecast_vol_ensemble is the main function that calls the above functions and returns the results of the ensemble forecast


############## RMSE IN LEAVE.ONE.OUT #######################
# Function to calculate the RMSE in Leave One Out cross-validation
rmse_LOO <- function(simulated_values, observed_values) {
  require(hydroGOF)
  
  # Check that the lengths of simulated and observed values match
  if (length(simulated_values) != length(observed_values)) {
    stop("Size of simulated values is NOT equal to observed values in RMSE")
  }
  # Initialize a matrix to store the RMSE values for each iteration
  rmse_matrix <- matrix(nrow = length(simulated_values))
  # Loop through each simulated value and calculate the RMSE 
  for (iterator in seq_along(simulated_values)) {
    rmse_matrix[iterator] <- hydroGOF::rmse(sim = simulated_values[-iterator],
                                            obs = observed_values[-iterator])
  }
  return(rmse_matrix)
}
############## REGRESSION
# Function for training the linear regression model
train_regression_model <- function(X_train, y_train,method,preProcess,...) {
  library(caret)
  #Train the linear regression model using the provided data and method
  train(
    X_train,
    y_train,
    metric = "RMSE",
    trControl = trainControl(method = "LOOCV", savePredictions = "all"),
    method = method,
    preProcess = preProcess,
    ...
  )
}

# Function for performing cross validation
cross_validation <- function(regression_model, y_train) {
  #Get the best model predictions and merge them with the actual values 
  y_best_model_pred = merge(
    regression_model$bestTune,
    regression_model$pred) %>%
    arrange(rowIndex)
  #Get the cross-validated simulated values
  y_cv                <- y_best_model_pred$pred
  
  # Calculate the RMSE in cross-validation
  rmse_cv              <- rmse_LOO(sim = y_cv,obs = y_train)
  rmse_model           <- hydroGOF::rmse(sim = y_cv, obs = y_train)
  
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
forecast_vol_determinist <- function(X_train, y_train, X_test,method='lm', preProcess = c("center", "scale"), mode = "both", ...) {
  # check mode
  if(!(mode %in% c("cv","prediction","both"))) stop("Invalid mode provided, please provide one of these cv,prediction,both.")
  #Train the regression model using the provided data and method
  regression_model <- train_regression_model(X_train, y_train, method, preProcess,...)
  # error from regression model
  rmse_model <- regression_model$results$RMSE

  #Initialise variables
  y_cv <- NULL
  rmse_cv <- NULL
  y_fore <- NULL
  
  if (mode == "both" || mode == "cv") {
    # Perform cross validation and get the results
    cv_results <- cross_validation(regression_model, y_train)
    y_cv <- cv_results$y_cv
    rmse_cv <- cv_results$rmse_cv
  }
  
  if (mode == "both" || mode == "prediction") {
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
  set.seed(10)
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
  
  ensemble_cv_and_test <- function(vol_deterministic, data_input, n_members, mode = c("both","cv", "prediction")) {
    # check mode
    if(!(mode %in% c("cv","prediction","both"))) stop("Invalid mode provided, please choose one of these: 'cv', 'prediction', 'both'")
    # Initialise ensemble variables
    y_ens_cv = NULL
    y_ens_fore = NULL
    
    if (mode == "both" || mode == "cv") {
      # Perform cross validation and get the results
      y_ens_cv = ensemble_cv(vol_deterministic, data_input, n_members)
    }
    if (mode == "both" || mode == "prediction") {
      # Make predictions on the test data
      y_ens_fore = ensemble_test(vol_deterministic, data_input, n_members)
    }
    
    return(c(y_ens_cv,y_ens_fore,vol_deterministic))
  }
  

forecast_vol_ensemble <- function(data_input,
                                  n_members=1000,
                                  method='lm',
                                  preProcess = c("center", "scale"),
                                  mode = data_input$info$mode
                                  ){
  # Train and predict using regression model
    vol_deterministic =
      forecast_vol_determinist(
        data_input$X_train,
        data_input$y_train$volume,
        data_input$X_test,
        method = 'lm',
        preProcess = preProcess,
        mode = mode
      )
   
    # m = vol_deterministic$regression_model$finalModel
    # pp = vol_deterministic$regression_model$preProcess
    # predict(m, predict(pp, data_input$X_test),se.fit = T)
    #https://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html
    
    # Generate ensemble forecast
    y_forecast = ensemble_cv_and_test(vol_deterministic, data_input, n_members,mode)

    return(y_forecast)
}
