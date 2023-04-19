
#weight function given a distance vector and menthod
calculate_weights <- function(neigh_dist,weight_method) {
  # check method
  if (!is.character(weight_method) || !weight_method %in% c("uniform", "distance", "ranking")) {stop("Invalid weight_method")}
  # Check weighting method and assign weight
  if (weight_method=='distance'){weight = neigh_dist}
  if (weight_method=='ranking'){weight = rank(neigh_dist)}
  if (weight_method=='uniform'){weight = rep(1, length(neigh_dist))}
  # calculate the Inverse Distance Weighting (IDW)
  idw <- function(x){(1/x)/sum(1/x)} 
  return(idw(weight))
}

#predict new variable given X (predictor) and f (target variable)
predict_new_f <- function(X_history,X_new,f_train,n_neighbours,weight_method ){
  # Get nearest neighbours
  # Compute Euclidean distance between the training and test set
  distance = apply(X_history, 1, function(x) {sqrt(sum((x - X_new) ^ 2))})
  # Order of the matrix rows based on distance
  distance_order = as.matrix(order(distance))
  
  # Variables for storing nearest neighbours
  neighbours_dist <- head(distance[distance_order],n_neighbours)
  neighbours_ind <- head(distance_order,n_neighbours)
  
  neighbours_wy <- head(names(neighbours_dist),n_neighbours) 
  neighbours_wy <- data.frame(matrix(neighbours_wy, nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
  rownames(neighbours_wy) = rownames(X_new)
  
  
  # Calculate weights
  weights = calculate_weights(neighbours_dist, weight_method)
  # "f" only for nearest neighbours
  f_neighbours = f_train[neighbours_ind,]
  # f_weighted
  f_weighted = weights %*% f_neighbours %>% as.data.frame()
  rownames(f_weighted) = rownames(X_new)
  
  return(
    list(
      neighbours_wy = neighbours_wy,
      f_weighted = f_weighted
    )
  )
}

ensemble_generator_q <- function(f,y_ens) {
  q_ens_i = list()
  
  num_ens = nrow(y_ens)
  num_wys_f = nrow(f)
  wys = colnames(y_ens)
  
  for (wy in wys) {
    
    if (num_ens == num_wys_f) {
      #ensemble=1 case
      q_ens_i[[wy]] =  as.matrix(f * y_ens)
    }else{
      #ensemble>1 case
      y_i = as.matrix(y_ens[,wy])
      f_i = as.matrix(f[wy,])
      
      q_ens_i[[wy]] = y_i %*% f_i
    }
  }
  return(q_ens_i)
}

knn_prediction <- function(X_train,f_train,n_neighbours,weight_method,y_ens_fore,X_test) {
  # normalise predictors X_train and X_test using range normalization method 
  pp = caret::preProcess(X_train, method = "range")
  X_train_minmax = predict(pp,X_train)
  X_test_minmax  = predict(pp,X_test)
  #predict new f for target year using k-NN algorithm with inputs:
  # X_history = normalized training predictors
  # X_new = normalized test predictors
  # f_train = training target variable
  # n_neighbours = number of nearest neighbours
  # weight_method = method to weight the neighbours
  f_new = 
    predict_new_f(
      X_history = X_train_minmax,
      X_new = X_test_minmax,
      f_train = f_train,
      n_neighbours = n_neighbours,
      weight_method = weight_method)
  # q= f*V, where V is volume forecast and f is the predicted target variable
  q_predict = ensemble_generator_q(f = f_new$f_weighted,y_ens = y_ens_fore)
  #return the computed flow and the nearest neighbours for the target year
  return(list(q = q_predict, wy_neighbours = f_new$neighbours_wy))
}

knn_cross_validation <- function(X_train,f_train,n_neighbours,weight_method,y_ens_cv) {
  # initialize variables to store predicted target variable and nearest neighbours for each water year
  f_cv_wy         = list()
  neighbours_wy_cv = list()
  
  # loop through water years on the training set
  wys = rownames(f_train)
  for (wy in wys) {
    # subset X_train using Leave-One-Out Cross-Validation (LOOCV)
    
    X_train_cv = subset(cbind(X_train),!(rownames(X_train) == wy))
    X_test_cv = subset(cbind(X_train),(rownames(X_train) == wy))
    
    
    # normalise predictors X_train and X_test using range normalization method
    pp = caret::preProcess(X_train_cv, method = "range")
    X_train_minmax_cv = predict(pp,X_train_cv)
    X_test_minmax_cv  = predict(pp,X_test_cv)
    #predict new f for target year using k-NN algorithm with inputs:
    # X_history = normalized training predictors
    # X_new = normalized test predictors
    # f_train = training target variable
    # n_neighbours = number of nearest neighbours
    # weight_method = method to weight the neighbours
    f_new = 
      predict_new_f(
        X_history = X_train_minmax_cv,
        X_new = X_test_minmax_cv,
        f_train = f_train,
        n_neighbours = n_neighbours,
        weight_method = weight_method)
    # store the predicted target variable and nearest neighbours for each water year
    f_cv_wy[[wy]] = f_new$f_weighted 
    neighbours_wy_cv[[wy]] = f_new$neighbours_wy
  }
  ## unlist f and neighbours-wys
  wy_neighbours_cv = rbindlist(neighbours_wy_cv,idcol = "wy")%>% column_to_rownames(var="wy")
  f_cv = rbindlist(f_cv_wy,idcol = "wy") %>% column_to_rownames(var="wy")
  # compute flow q = f*V, where V is volume forecast and f is the predicted target variable
  q_cv = ensemble_generator_q(f = f_cv,y_ens = y_ens_cv)
  #return the computed flow and the nearest neighbours for all water years
  return(list(q = q_cv, wy_neighbours = wy_neighbours_cv))
}


q_forecast <- function(q_train, y_train, X_train, X_test, y_ens_fore, y_ens_cv, n_neighbours, weight_method, forecast_mode) {
  set.seed(seed = 10)
  # save function arguments
  args <- as.list(environment())
  # check input
  if (!is.numeric(n_neighbours) || n_neighbours <= 0) {stop("Invalid n_neighbours. Number of neighbours should be a positive number")}
  if (!is.character(weight_method) || !weight_method %in% c("uniform", "distance")) {stop("Invalid weight_method. Should be either 'uniform' or 'distance'")}
  if (!is.character(forecast_mode) || !forecast_mode %in% c("cv", "prediction", "both")) {stop("Invalid mode. Should be either 'cv', 'prediction' or 'both'")}
  
  # target variables is f_i = Q_i/V of the forecast period (month i)
  f_train = rownames(q_train) %>% 
    lapply(function(x){q_train[x,]/y_train[x,'volume_original']}) %>%
    rbindlist() %>%
    as.matrix()
  rownames(f_train) = rownames(q_train)
  
  
  # initialise variables 
  q_predict = NULL
  wy_neighbours_predict = NULL
  q_cv = NULL
  wy_neighbours_cv = NULL
  
  # Prediction mode
  if (forecast_mode == "prediction" || forecast_mode == "both") {
    # perform flow prediction using k-NN algorithm with inputs:
    # X_train = training predictors
    # f_train = training target variable (flow/volume)
    # n_neighbours = number of nearest neighbours
    # weight_method = method to weight the neighbours
    # y_ens_fore = ensemble forecasts of volume
    # X_test = test predictors
    f_fore = knn_prediction(X_train,f_train,n_neighbours,weight_method,y_ens_fore,X_test)
    q_predict = f_fore$q
    wy_neighbours_predict = f_fore$wy_neighbours
  }
  
  # Cross-validation mode
  if (forecast_mode == "cv" || forecast_mode == "both") {    # perform leave-one-out cross-validation (LOOCV) on flow prediction using k-NN algorithm with inputs:
    # X_train = training predictors
    # f_train = training target variable (flow/volume)
    # n_neighbours = number of nearest neighbours
    # weight_method = method to weight the neighbours
    # y_ens_cv = ensemble forecasts of volume for cross-validation
    f_cv = knn_cross_validation(X_train,f_train,n_neighbours,weight_method,y_ens_cv)
    q_cv = f_cv$q
    wy_neighbours_cv = f_cv$wy_neighbours
  }


  return(list(q_cv = q_cv,
              wy_neighbours_cv = wy_neighbours_cv,
              q_predict = q_predict, 
              wy_neighbours_predict = wy_neighbours_predict,
              args = args
  ))
  
}

run_q_forecast <- function(data_input,
                           data_fore,
                           n_neighbours = 6,
                           weight_method="distance",
                           forecast_mode=data_input$info$forecast_mode) {
  
  data_input <- preprocess_data(
    datetime_initialisation = data_input$info$datetime_initialisation,
    forecast_mode = forecast_mode,
    catchment_code = data_input$info$catchment_code,
    predictor_list = unique(c("STORAGE_mean_1months","pr_sum_-1months",data_input$info$predictor_list)),
    save_raw = T
  )
  
  # perform flow prediction or cross-validation using the knn_forecast function
  # inputs:
  # q_train = training flow data
  # y_train = training volume data
  # X_train = training predictors
  # X_test = test predictors (used for prediction mode only)
  # y_ens_fore = ensemble forecasts of volume (used for prediction mode only)
  # y_ens_cv = ensemble forecasts of volume for cross-validation (used for cv mode only)
  # n_neighbours = number of nearest neighbours
  # weight_method = method to weight the neighbours
  # forecast_mode = "prediction", "cv", or "both"
  q_train = data_input$q_train
  y_train = data_input$y_train
  X_train = data_input$X_train
  X_test = data_input$X_test
  y_ens_fore = data_fore$y_ens_fore
  y_ens_cv = data_fore$y_ens_cv
  
  output = 
    q_forecast(
      q_train,
      y_train,
      X_train,
      X_test,
      y_ens_fore,
      y_ens_cv,
      n_neighbours,
      weight_method,
      forecast_mode
    )
  
  return(output)
}


