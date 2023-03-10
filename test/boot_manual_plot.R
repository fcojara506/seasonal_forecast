rm(list = ls())
# Generating data
set.seed(2021)
n <- 1000
x <- rnorm(n)
y <- x + rnorm(n)


#Models of population and sample
population.data <- as.data.frame(cbind(x, y))
population.model <- lm(y ~ x, population.data)
summary(population.model)


#Sampling the data
sample.data <-
  population.data[sample(nrow(population.data), 10, replace = TRUE), ]
sample.model <- lm(y ~ x, data = sample.data)
summary(sample.model)


#Plotting the models
plot(y ~ x, col = "gray", main = 'Population and Sample Regressions')
abline(coef(population.model)[1], coef(population.model)[2], col = "red")
abline(coef(sample.model)[1],
       coef(sample.model)[2],
       col = "blue",
       lty = 2)
legend(
  "topleft",
  legend = c("Sample", "Population"),
  col = c("red", "blue"),
  lty = 1:2,
  cex = 0.8)


#The bootstrap regression
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL

for (i in 1:1000) {
  sample_d = sample.data[sample(1:nrow(sample.data), nrow(sample.data), replace = TRUE), ]
  
  model_bootstrap <- lm(y ~ x, data = sample_d)
  
  sample_coef_intercept <-
    c(sample_coef_intercept, model_bootstrap$coefficients[1])
  
  sample_coef_x1 <-
    c(sample_coef_x1, model_bootstrap$coefficients[2])
}

coefs <- rbind(sample_coef_intercept, sample_coef_x1)


# ########

# Summarize the results
#summary(model)

model_coef <- function(data, index){
  coef(lm(y ~x, data = data, subset = index))
}
#model_coef(population.data, 1:47)
library(boot)
boot_coef = boot(population.data, model_coef, R=100)
#######

plot(boot_coef)
# Combining the results in a table
means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1))
knitr::kable(round(
  cbind(
    population = coef(summary(population.model))[, 1],
    sample = coef(summary(sample.model))[, 1],
    boot = apply(boot_coef$t, 2,mean) ,
    bootstrap = means.boot),10), 
  "simple", caption = "Coefficients in different models")



a <-
  cbind(
    quantile(sample_coef_intercept, prob = 0.025),
    quantile(sample_coef_intercept, prob = 0.975))
b <-
  cbind(quantile(sample_coef_x1, prob = 0.025),
        quantile(sample_coef_x1, prob = 0.975))

c <-
  round(cbind(
    population = confint(population.model),
    sample = confint(sample.model),
    bootpk = confint(boot_coef),
    boot = rbind(a, b)), 4)
colnames(c) <- c("2.5 %", "97.5 %",
                 "2.5 %", "97.5 %",
                 "2.5 %", "97.5 %",
                 "2.5 %", "97.5 %"
                 )
knitr::kable(rbind(
  c('population',
    'population',
    'sample',
    'sample',
    'boot',
    'boot',
    'bootstrap',
    'bootstrap'),c))

#Predicting on new data
new.data = seq(min(x), max(x), by = 0.05)
conf_interval <-
  predict(
    sample.model,
    newdata = data.frame(x = new.data),
    interval = "confidence",
    level = 0.95)


#Plotting the results on the project step-by-spet
plot(
  y ~ x,
  col = "gray",
  xlab = "x",
  ylab = "y",
  main = "Compare regressions")
apply(boot_coef$t %>% data.frame %>% t, 2, abline, col = rgb(1, 0, 0, 0.03))
abline(coef(population.model)[1], coef(population.model)[2], col = "blue")
abline(coef(sample.model)[1],
       coef(sample.model)[2],
       col = "black",
       lty = 2, lwd=3)
abline(mean(sample_coef_intercept),
       mean(sample_coef_x1),
       col = "green",
       lty = 4, lwd=3)
lines(new.data, conf_interval[, 2], col = "black", lty = 3, lwd=3)
lines(new.data, conf_interval[, 3], col = "black", lty = 3, lwd=3)
legend("topleft",
       legend = c("Bootstrap", "Population", 'Sample'),
       col = c("red", "blue", 'green'),
       lty = 1:3,
       cex = 0.8)


