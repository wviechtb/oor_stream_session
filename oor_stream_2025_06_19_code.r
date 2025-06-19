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
# with a model without any predictor, then one predictor, two predictors, all
# the way up to all 4 predictors

fitmodels <- function(dat, betasd=1) {

   res1 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a,
                      a ~ dnorm(0, 1)), data=dat)
   res2 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, betasd)), data=dat, start=list(b=rep(0,1)))
   res3 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1 + b[2]*X2,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, betasd)), data=dat, start=list(b=rep(0,2)))
   res4 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1 + b[2]*X2 + b[3]*X3,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, betasd)), data=dat, start=list(b=rep(0,3)))
   res5 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1 + b[2]*X2 + b[3]*X3 + b[4]*X4,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, betasd)), data=dat, start=list(b=rep(0,4)))
   res <- list(res1, res2, res3, res4, res5)
   return(res)

}

# now we repeat the simulation described (note: this takes quite a bit of
# time, so the code prints the iteration number to keep track of progress;
# running this with 100 iterations is already sufficient to see the same
# pattern as shown in the book)

iters <- 100

dev.train <- matrix(NA_real_, nrow=iters, ncol=5)
dev.test  <- matrix(NA_real_, nrow=iters, ncol=5)

n <- 20

for (j in 1:iters) {

   print(j)
   dat <- simdata(n)
   res <- fitmodels(dat)
   lppd <- sapply(res, function(m) sum(lppd(m)))
   dev.train[j,] <- -2 * lppd
   dat <- simdata(n)
   lppd <- sapply(res, function(m) sum(lppd(m, data=dat)))
   dev.test[j,] <- -2 * lppd

}

dev.train.mean <- apply(dev.train, 2, mean)
dev.train.lo   <- dev.train.mean - apply(dev.train, 2, sd)
dev.train.hi   <- dev.train.mean + apply(dev.train, 2, sd)

dev.test.mean  <- apply(dev.test, 2, mean)
dev.test.lo    <- dev.test.mean - apply(dev.test, 2, sd)
dev.test.hi    <- dev.test.mean + apply(dev.test, 2, sd)

# Figure 7.6 (left): deviance in and out of sample for the 5 models for n=20
plot(NA, xlim=c(0.8,5.2), ylim=c(min(dev.train.lo, dev.test.lo), max(dev.train.hi, dev.test.hi)),
     xlab="number of parameters", ylab="deviance", main=paste("N =", n))
segments(1:5, dev.train.lo, 1:5, dev.train.hi, col="#1e59ae", lwd=2)
points(1:5, dev.train.mean, pch=19, col="#1e59ae")
segments(1:5 + 0.1, dev.test.lo, 1:5 + 0.1, dev.test.hi, lwd=2)
points(1:5 + 0.1, dev.test.mean, pch=21)
