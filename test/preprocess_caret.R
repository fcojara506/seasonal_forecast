# Function for preprocessing data before training
preprocess_before_train <- function(x, y, preProcMethod, test_set) {
  # Preprocess data
  preProcData <- preProcess(x, method = preProcMethod)
  
  # Get preprocessed data
  x_pp <- predict(preProcData, newdata = x)
  
  # Remove missing values
  x_pp <- na.omit(x_pp)
  y <- na.omit(y)
  
  # Train model using preprocessed data
  model <- train(x_pp, y, method = "glm")
  
  # predict on test set
  pred <- predict(model, newdata = predict(preProcData, newdata = test_set))

  
  return(pred)
}

# Function for preprocessing data within train function
preprocess_within_train <- function(x, y, preProcMethod, test_set) {
  # Remove missing values
  x <- na.omit(x)
  y <- na.omit(y)
  
  # Train model using preProcess within train
  model <- train(x, y, method = "glm",
                 preProcess = preProcMethod)
  

  
  
  return(pred)
}

# Split data into x and y
x <- mtcars[, -1]
y <- mtcars[, 1]

# Split data into training and testing set
splitIndex <- createDataPartition(y, p = .7, list = FALSE, times = 1)
x_train <- x[splitIndex,]
y_train <- y[splitIndex]
x_test <- x[-splitIndex,]
y_test <- y[-splitIndex]

# Preprocess data before and within train function
cm1 <- preprocess_before_train(x_train, y_train, c("center", "scale"), x_test)
cm2 <- preprocess_within_train(x_train, y_train, c("center", "scale"), x_test)

# Compare the performance
identical(cm1,cm2)
