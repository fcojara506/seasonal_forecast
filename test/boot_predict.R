rm(list = ls())
library(caret)
library(boot)

# In this example we show the use of boot in a prediction from
# regression based on the nuclear data.  This example is taken
# from Example 6.8 of Davison and Hinkley (1997).  Notice also
# that two extra arguments to 'statistic' are passed through boot.
nuke <- nuclear[, c(1, 2, 5, 7, 8, 10, 11)]
nuke.lm <- glm(log(cost) ~ date+log(cap)+ne+ct+log(cum.n)+pt, data = nuke)
nuke.lm2 <-train(log(cost) ~ date+log(cap)+ne+ct+log(cum.n)+pt,
                 data = nuke,
                 method="lm",
                 trControl =trainControl(savePredictions = 'all',method = "LOOCV"))

nuke.lm2

nuke.diag <- glm.diag(nuke.lm)
nuke.res <- nuke.diag$res
#nuke.res <- nuke.diag$res * nuke.diag$sd
#nuke.res <- nuke.res - mean(nuke.res)

glm.diag.plots(glmfit = nuke.lm)
# We set up a new data frame with the data, the standardized 
# residuals and the fitted values for use in the bootstrap.
nuke.data <- data.frame(nuke, resid = nuke.res, fit = fitted(nuke.lm),logcost = log(nuke$cost))

# Now we want a prediction of plant number 32 but at date 73.00
new.data <- data.frame(cost = 1, date = 73.00, cap = 886, ne = 0, ct = 0, cum.n = 11, pt = 1)
new.fit <- predict(nuke.lm, new.data)

nuke.fun <- function(dat, inds, i.pred, fit.pred, x.pred)
{
  print(paste0(dat$fit+dat$resid[inds],'_',dat$logcost[inds]))
  lm.b <- glm(fit+resid[inds] ~ date+log(cap)+ne+ct+log(cum.n)+pt, data = dat)
  pred.b <- predict(lm.b, x.pred)
  c(coef(lm.b), pred.b - (fit.pred + dat$resid[i.pred]))
}

nuke.boot <- boot(nuke.data, nuke.fun, R = 999, m = 1, 
                  fit.pred = new.fit, x.pred = new.data)
# The bootstrap prediction squared error would then be found by
mean(nuke.boot$t[, 8]^2)
mean(nuke.boot$t[, 8]^2)

# Basic bootstrap prediction limits would be
new.fit - sort(nuke.boot$t[, 8])[c(975, 25)]
