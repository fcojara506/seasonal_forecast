
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

nearest_neighbors <- function(X_train,X_test,n_neighbors=10) {
  
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

#weight function given a distance vector and menthod
weighting_method <- function(neigh_dist,
                             weight_method=c('distance','ranking','uniform')
                             ) {
  
 if (weight_method=='distance') {weight = neigh_dist}
 if (weight_method=='ranking') {weight = rank(neigh_dist)}
 if (weight_method=='uniform') {weight = rep(1, length(neigh_dist))}
  # inverse to the x function
  idw <- function(x){(1/x)/sum(1/x)} 
  return(idw(weight))
}
   
#predict new variable given X (predictor) and f (target variable)
knn_predict <- function(X_train,
                        X_test,
                        f_train,
                        n_neighbors=10,
                        weight_method = c('distance','ranking','uniform')
                        ){
  
  neigh_distance = nearest_neighbors(X_train,
                                     X_test,
                                     n_neighbors = n_neighbors)
  
  weights = weighting_method(neigh_distance$neigh_dist,
                            weight_method = weight_method)
  
  ## "f" only for nearest neighbours
  f_neigh = f_train[neigh_distance$neigh_ind,]
  

  f_predict = weights %*% f_neigh
  rownames(f_predict) = rownames(X_test)

  return(
    list(
      neigh_distance = neigh_distance,
      weight = weights,
      f_neigh = f_neigh,
      f_predict = f_predict
  )
  )
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

knn_model <- function(data_input,
                      n_neighbors = 10,
                      weight_method = c('distance','ranking','uniform'),
                      loocv=F
                      ) {
  library(caret)
  
  # target variables is f_i = Q_i/V of the forecast period (month i)
  f_train = lapply(
    rownames(data_input$q_train),
    function(x){data_input$q_train[x,]/data_input$y_train[x,]}
    ) %>%
    rbindlist() %>%
    as.matrix()
  
  rownames(f_train) = rownames(data_input$q_train)
  
  wys = rownames(f_train)
  ensemble_names = names(data_input$X_train)
  f_fore         = vector(mode = "list",length = length(ensemble_names))
  names(f_fore)  = ensemble_names

  f_cv_wy         = list()
  f_cv            = list()
  for (ens_i in ensemble_names) {

  # normalise predictor data_input  
  pp = caret::preProcess(data_input$X_train[[ens_i]], method = "range")
  X_train_minmax = predict(pp,data_input$X_train[[ens_i]])
   if (! is.null(vol_det_unique$y_fore) & data_input$info$test_subset) {
    X_test_minmax  = predict(pp,data_input$X_test[[ens_i]])
  }else{X_test_minmax = NULL}
  

  
  f_fore[[ens_i]] = 
    knn_predict(
    X_train = X_train_minmax,
    X_test = X_test_minmax,
    f_train = f_train,
    n_neighbors = n_neighbors,
    weight_method = weight_method)$f_predict %>% 
    as.data.frame() %>% 
    mutate(wy = data_input$wy_holdout)
  
  if (loocv) {
    for (wy in wys) {
      X_train_minmax_cv = subset(X_train_minmax,!(rownames(X_train_minmax) %in% wy))
      X_test_minmax_cv = subset(X_train_minmax,(rownames(X_train_minmax) %in% wy))
      
      f_cv_wy[[wy]] = 
        knn_predict(
          X_train = X_train_minmax_cv,
          X_test = X_test_minmax_cv,
          f_train = f_train,
          n_neighbors = n_neighbors,
          weight_method = weight_method)$f_predict %>% 
        as.data.table() %>% 
        mutate(wy = wy)
      
    }
    
    f_cv[[ens_i]] = rbindlist(f_cv_wy) 
  }
  
  }
  
  f_fore = rbindlist(f_fore) %>% 
    data.frame() %>% 
    column_to_rownames(var="wy")
  
  f_cv = rbindlist(f_cv) %>%
    data.frame() %>% 
    column_to_rownames(var="wy")
  
  return(
    list(
      f_fore=f_fore,
      f_cv = f_cv
      )
  )
  }

q_ensemble <- function(data_input,
                       data_fore,
                       n_neighbors = 6,
                       weight_method = 'distance',
                       loocv=F) {
  
  
  
  
  # we use q = f*V to get q ensemble
  model = 
    knn_model(
    data_input = data_input,
    n_neighbors = n_neighbors,
    weight_method = weight_method,
    loocv = loocv
    )
  
  #f:q/V from knn
  f_fore = model$f_fore
  #V: volume forecast
  y_ens_fore = data_fore$y_ens_fore
  # q= f*V
  q_fore = ensemble_generator_q(f = f_fore,y_ens = y_ens_fore)
  
  if (loocv) {
    #f:q/V from knn
    f_cv = model$f_cv
    #V: volume forecast
    y_ens_cv = data_fore$y_ens_cv
    q_cv = ensemble_generator_q(f = f_cv,y_ens = y_ens_cv)
    return(
      list(
      q_fore = q_fore,
      q_cv = q_cv 
    ))
  }else{
    return(
      list(q_fore = q_fore)
    )
  }  
}