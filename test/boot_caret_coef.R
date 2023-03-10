rm(list = ls())
library(caret)
library(boot)

n <- 500L

x1 <- rnorm(n, 2.0, 0.5)
x2 <- rnorm(n, -1.0, 2)
  y <- factor(rbinom(n, 1L, plogis(-0.6 + 1.0 * x1 - 0.8 * x2)))

dat <- data.frame(y, x1, x2)

caretMod <- train(y ~ ., 
                  data = dat, 
                  method = "glmnet",
                  trControl = trainControl(method = "CV"))

caretMod

bootSamples <- boot(dat, function(data, idx) {
  bootstrapData <- data[idx, ]
  bootstrapMod <- train(y ~ ., 
                        data = bootstrapData, 
                        method = "glmnet", 
                        trControl = trainControl(method = "none"),
                        tuneGrid = caretMod$bestTune)
  
  as.vector(coef(bootstrapMod$finalModel, caretMod$bestTune$lambda))
}, 1000L,ncpus = 4)
bootSamples
