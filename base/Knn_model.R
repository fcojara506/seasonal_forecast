# Function to calculate the Euclidean distance between each row of the matrix and the reference vector
calculate_euclidean_distance <- function(matrix, reference_vector) {
  # Initialize distance variable
  distance = apply(matrix, 1, function(x) {
    # Calculate Euclidean distance
    sqrt(sum((x - reference_vector) ^ 2))
  })
  # Return distance and order of the matrix rows based on distance
  return(list(distance = distance, order = as.matrix(order(distance))))
}

# Function to find the nearest neighbors of a test point in the training set
find_nearest_neighbors <- function(X_train,X_new,n_neighbors=10) {
  # Compute Euclidean distance between the training and test set
  dis_ind = calculate_euclidean_distance(X_train,X_new)
  # Initialize variables for storing nearest neighbors
  neigh_dist <- head(dis_ind$distance[dis_ind$order],n_neighbors)
  neigh_ind <- head(dis_ind$order,n_neighbors)
  neigh_wy <- head(names(dis_ind),n_neighbors)
  # Return distances, indices, and names of the nearest neighbors
  return(list(neigh_dist = neigh_dist, neigh_ind = neigh_ind, neigh_wy = neigh_wy))
}

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
predict_new_f <- function(X_train,
                        X_new,
                        f_train,
                        n_neighbors=10,
                        weight_method = c('distance','ranking','uniform')
                        ){
  # Get nearest neighbors
  neigh_distance = find_nearest_neighbors(X_train,X_new, n_neighbors)
  # Calculate weights
  weights = calculate_weights(neigh_distance$neigh_dist, weight_method)
  ## "f" only for nearest neighbours
  f_neigh = f_train[neigh_distance$neigh_ind,]
  # Predict f
  f_predict = weights %*% f_neigh
  rownames(f_predict) = rownames(X_new)

  return(
    list(
      neigh_distance = neigh_distance,
      weight = weights,
      f_neigh = f_neigh,
      f_predict = f_predict
  )
  )
}

preprocess_data <- function(q_train, y_train, X_train, X_test, mode) {
  # target variables is f_i = Q_i/V of the forecast period (month i)
  f_train = lapply(
    rownames(q_train),
    function(x){q_train[x,]/y_train[x,]}
  ) %>%
    rbindlist() %>%
    as.matrix()
  
  rownames(f_train) = rownames(q_train)
  
  # normalise predictor data_input  
  pp = caret::preProcess(X_train, method = "range")
  X_train_minmax = predict(pp,X_train)
  
  X_test_minmax = NULL
  if (mode == "prediction" || mode == "both") {
    X_test_minmax  = predict(pp,X_test)
  }
  
  return(list(X_train_minmax = X_train_minmax, X_test_minmax = X_test_minmax, f_train = f_train))
}

forecast_mode <- function(X_train_minmax, X_test_minmax, f_train, n_neighbors, weight_method, mode) {
  
  # Input validation
  if (is.null(X_test_minmax) && mode %in% c("both","prediction")) {stop("Invalid X_test_minmax")}
  if (!is.numeric(n_neighbors) || n_neighbors <= 0) {stop("Invalid n_neighbors")}
  if (!is.character(mode) || !mode %in% c("cv", "prediction", "both")) {stop("Invalid mode")}
  # initialise variables 
  f_target = NULL
  # Forecast for the holdout period
  if (mode == "prediction" || mode == "both") {
    f_target = 
      predict_new_f(
        X_train = X_train_minmax,
        X_new = X_test_minmax,
        f_train = f_train,
        n_neighbors = n_neighbors,
        weight_method = weight_method)$f_predict %>% 
      as.data.frame()
  }
  
  ## cross validation
  f_cv_wy         = list()
  f_cv = NULL
  
  if (mode == "cv" || mode == "both") {
    wys = rownames(f_train)
    for (wy in wys) {
      X_train_minmax_cv = subset(X_train_minmax,!(rownames(X_train_minmax) == wy))
      X_test_minmax_cv = subset(X_train_minmax,(rownames(X_train_minmax) == wy))
      
      f_cv_wy[[wy]] = 
        predict_new_f(
          X_train = X_train_minmax_cv,
          X_new = X_test_minmax_cv,
          f_train = f_train,
          n_neighbors = n_neighbors,
          weight_method = weight_method)$f_predict %>% 
        as.data.table() %>% 
        mutate(wy = wy)
    }
    
    f_cv = rbindlist(f_cv_wy) %>% column_to_rownames(var="wy")
  }
  
  return(list(f_target = f_target, f_cv = f_cv))
}

f_prediction <- function(q_train, y_train, X_train, X_test, n_neighbors, weight_method, mode, seed = 10) {
  # check input
  if (!is.numeric(n_neighbors) || n_neighbors <= 0) {stop("Invalid n_neighbors")}
  if (!is.character(weight_method) || !weight_method %in% c("uniform", "distance")) {stop("Invalid weight_method")}
  if (!is.character(mode) || !mode %in% c("cv", "prediction", "both")) {stop("Invalid mode")}
  set.seed(seed)
  
  preprocessed_data <- preprocess_data(q_train, y_train, X_train,X_test, mode)
  
  output <- forecast_mode(X_train_minmax = preprocessed_data$X_train_minmax,
                          X_test_minmax = preprocessed_data$X_test_minmax,
                          f_train = preprocessed_data$f_train,
                          n_neighbors = n_neighbors,
                          weight_method = weight_method,
                          mode = mode)
  
  return(output)
}

ensemble_generator_q <- function(f,y_ens) {
  q_ens_i = list()
  #
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

q_ensemble <- function(data_input,
                       data_fore,
                       n_neighbors = 6,
                       weight_method = 'distance',
                       mode = "both") {
  
  # we use q = f*V to get q ensemble
  f = f_prediction(
    q_train = data_input$q_train,
    y_train = data_input$y_train,
    X_train = data_input$X_train,
    X_test = data_input$X_test,
    n_neighbors = n_neighbors,
    weight_method = weight_method,
    mode = mode
  )
  #prediction mode
  q_fore = NULL
  if (mode == "prediction" || mode == "both") {
  #f:q/V from knn
  f_target = f$f_target
  #V: volume forecast
  y_ens_fore = data_fore$y_ens_fore
  # q= f*V
  q_fore = ensemble_generator_q(f = f_target,y_ens = y_ens_fore)
  }
  
  ## cross validation
  q_cv = NULL
  if (mode == "cv" || mode == "both") {
    #f:q/V from knn
    f_cv = f$f_cv
    #V: volume forecast
    y_ens_cv = data_fore$y_ens_cv
    q_cv = ensemble_generator_q(f = f_cv,y_ens = y_ens_cv)
  }  
  return(list(q_fore = q_fore,q_cv = q_cv ))
}