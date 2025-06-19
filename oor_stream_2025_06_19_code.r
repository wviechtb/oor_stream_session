############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-06-19
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 7.3 - ?
#
# last updated: 2025-06-19

############################################################################

# load the rethinking package
library(rethinking)

############################################################################

### 7.3: Golem taming: regularization

# Figure 7.7: plots of the density of a normal distribution with mean 0 and
# standard deviations of 1, 0.5, and 0.2
plot(NA, xlim=c(-3,3), ylim=c(0,2), xlab="parameter value", ylab="density",
     bty="l", las=1)
curve(dnorm(x, mean=0, sd=1),   from=-3, to=3, n=1001, add=TRUE, lty="dashed")
curve(dnorm(x, mean=0, sd=0.5), from=-3, to=3, n=1001, add=TRUE)
curve(dnorm(x, mean=0, sd=0.2), from=-3, to=3, n=1001, add=TRUE, lwd=3)

# function to simulate data based on the model given on page 212 (note: only
# predictors 1 and 2 actually are related to the outcomes)

simdata <- function(n) {
   X <- replicate(4, rnorm(n))
   mu <- 0.15 * X[,1] - 0.4 * X[,2]
   y <- rnorm(n, mean=mu, sd=1)
   return(data.frame(X, y))
}

# function to the fit 5 regression models of increasing complexity, starting
# with a model without any predictors (just an intercept), then one predictor,
# then two predictors, all the way up to all 4 predictors; here, we use vague
# priors on the slopes (SD=10)

fitmodels <- function(dat) {

   res1 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a,
                      a ~ dnorm(0, 1)), data=dat)
   res2 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, 10)), data=dat, start=list(b=rep(0,1)))
   res3 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1 + b[2]*X2,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, 10)), data=dat, start=list(b=rep(0,2)))
   res4 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1 + b[2]*X2 + b[3]*X3,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, 10)), data=dat, start=list(b=rep(0,3)))
   res5 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1 + b[2]*X2 + b[3]*X3 + b[4]*X4,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, 10)), data=dat, start=list(b=rep(0,4)))
   res <- list(res1, res2, res3, res4, res5)
   return(res)

}

iters <- 2
n <- 20

dev.train <- matrix(NA_real_, nrow=iters, ncol=5)
dev.test  <- matrix(NA_real_, nrow=iters, ncol=5)
dev.cv    <- matrix(NA_real_, nrow=iters, ncol=5)

for (j in 1:iters) {

   print(j)
   dat <- simdata(n)
   res <- fitmodels(dat)
   lppd <- sapply(res, function(m) sum(lppd(m)))
   dev.train[j,] <- -2 * lppd
   lppd.cv <- c(0,0,0,0,0)
   for (k in 1:n) {
      res <- fitmodels(dat[-k,])
      lppd.cv <- lppd.cv + sapply(res, function(m) lppd(m, data=dat[k,]))
   }
   dev.cv[j,] <- -2 * lppd.cv
   dat <- simdata(n)
   lppd <- sapply(res, function(m) sum(lppd(m, data=dat)))
   dev.test[j,] <- -2 * lppd

}

dev.train.mean.vague <- apply(dev.train, 2, mean)
dev.test.mean.vague  <- apply(dev.test, 2, mean)
dev.cv.mean.vague    <- apply(dev.cv, 2, mean)

# Figure 7.8: deviance in and out of sample for the 5 models
plot(NA, xlim=c(0.8,5.2), ylim=range(dev.train.mean.vague, dev.test.mean.vague),
     xlab="number of parameters", ylab="deviance", main=paste("N =", n))
points(1:5, dev.train.mean.vague, pch=19, col="#1e59ae")
points(1:5, dev.test.mean.vague, pch=21)

# now we repeat the same as above, but using SD=0.2 for the slope priors

fitmodels <- function(dat) {

   res1 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a,
                      a ~ dnorm(0, 1)), data=dat)
   res2 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, 0.5)), data=dat, start=list(b=rep(0,1)))
   res3 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1 + b[2]*X2,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, 0.5)), data=dat, start=list(b=rep(0,2)))
   res4 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1 + b[2]*X2 + b[3]*X3,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, 0.5)), data=dat, start=list(b=rep(0,3)))
   res5 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1 + b[2]*X2 + b[3]*X3 + b[4]*X4,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, 0.5)), data=dat, start=list(b=rep(0,4)))
   res <- list(res1, res2, res3, res4, res5)
   return(res)

}

dev.train <- matrix(NA_real_, nrow=iters, ncol=5)
dev.test  <- matrix(NA_real_, nrow=iters, ncol=5)
dev.cv    <- matrix(NA_real_, nrow=iters, ncol=5)

for (j in 1:iters) {

   print(j)
   dat <- simdata(n)
   res <- fitmodels(dat)
   lppd <- sapply(res, function(m) sum(lppd(m)))
   dev.train[j,] <- -2 * lppd
   lppd.cv <- c(0,0,0,0,0)
   for (k in 1:n) {
      res <- fitmodels(dat[-k,])
      lppd.cv <- lppd.cv + sapply(res, function(m) sum(lppd(m, data=dat[k,])))
   }
   dev.cv[j,] <- -2 * lppd.cv
   dat <- simdata(n)
   lppd <- sapply(res, function(m) sum(lppd(m, data=dat)))
   dev.test[j,] <- -2 * lppd

}

dev.train.mean.ridge <- apply(dev.train, 2, mean)
dev.test.mean.ridge  <- apply(dev.test, 2, mean)
dev.cv.mean.ridge    <- apply(dev.cv, 2, mean)

# add the lines for the deviance from these models to the plot
lines(1:5, dev.train.mean.ridge, col="#1e59ae", lwd=3)
lines(1:5, dev.test.mean.ridge, lwd=3)

############################################################################

## Rethinking: Ridge regression

# simulate some new data based on n=20 and standardize all variables
set.seed(1234)
dat <- simdata(n=20)
dat <- data.frame(apply(dat, 2, scale))

# fit the most complex model with very vague priors on the intercept and slopes
res5 <- quap(alist(y ~ dnorm(mu, sigma),
                  mu <- a + b[1]*X1 + b[2]*X2 + b[3]*X3 + b[4]*X4,
                  a ~ dnorm(0, 10),
                  b ~ dnorm(0, 10),
                  sigma ~ dexp(1)), data=dat, start=list(b=rep(0,4)))
precis(res5, depth=2)

# compare this against just using lm()
round(coef(summary(lm(y ~ X1 + X2 + X3 + X4, data=dat))), digits=2)

# fit the same model with lm.ridge(), i.e., using lambda=0
library(MASS)
res <- lm.ridge(y ~ X1 + X2 + X3 + X4, data=dat, lambda=0)
round(coef(res), digits=2)

# fit the most complex model with a fairly strict prior on the slopes
res5 <- quap(alist(y ~ dnorm(mu, sigma),
                  mu <- a + b[1]*X1 + b[2]*X2 + b[3]*X3 + b[4]*X4,
                  a ~ dnorm(0, 10),
                  b ~ dnorm(0, 0.2),
                  sigma ~ dexp(1)), data=dat, start=list(b=rep(0,4)))
precis(res5, depth=2)

# use lm.ridge() with a lambda value that gives the same results as the
# Bayesian model (note: found lambda here by trial and error)
res <- lm.ridge(y ~ X1 + X2 + X3 + X4, data=dat, lambda=19.0205)
round(coef(res), digits=2)

############################################################################

### 7.4: Predicting predictive accuracy

## 7.4.1: Cross-validation

# Figure 7.8: deviance in and out of sample for the 5 models
plot(NA, xlim=c(0.8,5.2), ylim=range(dev.test.mean.vague, dev.cv.mean.vague, dev.test.mean.ridge, dev.cv.mean.ridge),
     xlab="number of parameters", ylab="deviance", main=paste("N =", n))
points(1:5, dev.test.mean.vague, pch=21)
points(1:5, dev.test.mean.ridge, pch=19)

lines(1:5, dev.cv.mean.vague, col="#1e59ae", lwd=3)
lines(1:5, dev.cv.mean.vague, col="#1e59ae", lwd=3)

