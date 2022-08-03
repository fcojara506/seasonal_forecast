
distance_p <- function(matrix, reference_vector) {
  
  distance = apply(
    matrix, 1, 
    function(x) {
    sqrt(sum((x - reference_vector) ^ 2))
  })
  
  return(
    list(
    distance = distance,
    order = as.matrix(order(distance))
    )
    )
}

neighbors <- function(X_train,X_test,n_neighbors=10) {
  
  dis_ind = distance_p(X_train,X_test)
  
  distance_n_neighbors = head(dis_ind$distance[dis_ind$order],n_neighbors)
  index_n_neighbors = head(dis_ind$order,n_neighbors)
  wy_n_neigbors = head(names(distance_n_neighbors),n_neighbors)
  
  return(
    list(
      neigh_dist = distance_n_neighbors,
      neigh_ind = index_n_neighbors,
      neigh_wy = wy_n_neigbors
    )
  )
}
# inverse to the x function
idw <- function(x){(1/x)/sum(1/x)} 

#weight function given a distance vector and menthod
weighting_method <- function(neigh_dist,weight_method='distance') {
 if (weight_method=='distance') {weight = neigh_dist}
 if (weight_method=='ranking') {weight = rank(neigh_dist)}
 if (weight_method=='uniform') {weight = rep(1, length(neigh_dist))}
return(idw(weight))
}
   
#predict new variable given X (predictor) and f (target variable)
knn_predict <- function(X_train,
                        X_test,
                        f_train,
                        n_neighbors=10,
                        weight_method = 'distance'
                        ){
  
  neigh_distance = neighbors(X_train,X_test,n_neighbors = n_neighbors)
  weight = weighting_method(neigh_distance$neigh_dist,weight_method = weight_method)
  f_neigh = f_train[neigh_distance$neigh_ind,]
  f_predict = weight%*%f_neigh 
  rownames(f_predict) = rownames(X_test)
  
  return(
    list(
      neigh_distance = neigh_distance,
      weight = weight,
      f_neigh = f_neigh,
      f_predict = f_predict
  )
  )
}


ensemble_generator_q <- function(f,y_ens) {
  #y_ens_i = list()
  
  nrow_y = nrow(y_ens)
  nrow_f = nrow(f)
  
  if (nrow_y == nrow_f) {
    y_ens_i =  as.matrix(f * y_ens)
  }else{
  y_i = as.matrix(y_ens)
  f_i = as.matrix(f)
  y_ens_i = y_i %*% f_i
  }
  

  
  
  #
  
  return(y_ens_i)
}

knn_model <- function(data,
                      n_neighbors = 10,
                      weight_method = 'distance'
                      ) {
  library(caret)
  
  # target variables is f_i = Q_i/V of the forecast period (month i)
  f_train = t(apply(data$q_train,1, function(x) x/sum(x)))
  
  # normalise predictor data
  ensemble_names = names(data$X_train)
  
  f_fore         = vector(mode = "list",length = length(ensemble_names))
  names(f_fore)  = ensemble_names
  
  for (ens_i in ensemble_names) {
    
  pp = caret::preProcess(data$X_train[[ens_i]], method = "range")
  X_train_minmax = predict(pp,data$X_train[[ens_i]])
  X_test_minmax  = predict(pp,data$X_test[[ens_i]])
  
  f_fore[[ens_i]] = 
    knn_predict(
    X_train = X_train_minmax,
    X_test = X_test_minmax,
    f_train = f_train,
    n_neighbors = n_neighbors,
    weight_method = weight_method)$f_predict %>% 
    as.data.frame()
  }
  
  f_fore = rbindlist(f_fore)
  return(f_fore)
  }

q_ensemble <- function(data,data_fore,...) {
  
  # we use q = f*V to get q ensemble
  f_fore = 
    knn_model(
    data = data,
    n_neighbors = 6,
    weight_method = 'distance'
    )
  
  # volume forecast
  y_ens_fore = data_fore$y_ens_fore
  
  # q= f*V
  q_fore = 
    ensemble_generator_q(
    f = f_fore,
    y_ens = y_ens_fore
    )
  
  return(q_fore)
}



