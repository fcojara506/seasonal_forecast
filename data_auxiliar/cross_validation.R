"fco_cv"<- function(x,y,theta.fit,theta.predict,...,ngroup=n){
  call <- match.call()
  x1 <- as.matrix(x)
  y  <- as.matrix(y)
  
  n <- length(y)
  ngroup <- trunc(ngroup)
  if( ngroup < 2){
    stop ("ngroup should be greater than or equal to 2")
  }
  if(ngroup > n){
    stop ("ngroup should be less than or equal to the number of observations")
  }
  
  if(ngroup==n) {groups <- 1:n; leave.out <- 1}
  if(ngroup<n){
    leave.out <- trunc(n/ngroup);
    o <- sample(1:n)
    groups <- vector("list",ngroup)
    for(j in 1:(ngroup-1)){
      jj <- (1+(j-1)*leave.out)
      groups[[j]] <- (o[jj:(jj+leave.out-1)])
    }
    groups[[ngroup]] <- o[(1+(ngroup-1)*leave.out):n]
  }
  u <- vector("list",ngroup)
  cv.fit <- rep(NA,n)
  cv.se <- rep(NA,n)
  for(j in 1:ngroup){
    
    u  <- theta.fit(x1[-groups[[j]], ]   , y[-groups[[j]]] )
    x0 <- list(x[groups[[j]],])
    results=  theta.predict(u,x0)
    cv.fit[groups[[j]]] <-  results$fit
    #cv.se[groups[[j]]] <-  results$se.fit
  }
  
  if(leave.out==1) groups <- NULL
  return(list(cv.fit=cv.fit,
              
              #ngroup=ngroup, 
              #leave.out=leave.out,
              groups_HeldOut=groups, 
              call=call,
              training_data=list(predictors=x, predictand=y)))
}



"fco_cv_locfit"<- function(data_train_cuenca,theta.fit,theta.predict,...,ngroup=n){
  
  x=data_train_cuenca %>% select(-volumen) %>% as.matrix()
  y=data_train_cuenca %>% select(volumen) %>% as.matrix()
  
  n <- length(y)
  
  ngroup <- trunc(ngroup)
  
  if( ngroup < 2){
    stop ("ngroup should be greater than or equal to 2")
  }
  if(ngroup > n){
    stop ("ngroup should be less than or equal to the number of observations")
  }
  
  if(ngroup==n) {groups <- 1:n; leave.out <- 1}
  
  if(ngroup<n){
    leave.out <- trunc(n/ngroup);
    o <- sample(1:n)
    groups <- vector("list",ngroup)
    for(j in 1:(ngroup-1)){
      jj <- (1+(j-1)*leave.out)
      groups[[j]] <- (o[jj:(jj+leave.out-1)])
    }
    groups[[ngroup]] <- o[(1+(ngroup-1)*leave.out):n]
  }
  
  u <- vector("list",ngroup)
  cv.fit <- rep(NA,n)
  cv.se <- rep(NA,n)
  
  
  
  for(j in 1:ngroup){
    
    u  <- theta.fit(data_train_cuenca =  data_train_cuenca[-groups[[j]], ])
    x0 <- data_train_cuenca[groups[[j]], ]
    results=  theta.predict(fitted=u,x0)
    cv.fit[groups[[j]]] <-  results
    #cv.se[groups[[j]]] <-  results$se.fit
  }
  
  if(leave.out==1) groups <- NULL
  return(list(cv.fit=cv.fit,
              groups_HeldOut=groups, 
              training_data=list(predictors=x, predictand=y)))
}