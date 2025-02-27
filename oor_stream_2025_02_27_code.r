############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-02-27
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 5.1 - ?
#
# last updated: 2025-02-27

############################################################################

### 5.1: Spurious association

# load the rethinking package
library(rethinking)

# get the WaffleDivorce dataset and put it into 'dat'
dat <- get(data(WaffleDivorce))

# Figure 5.2
par(mfrow=c(1,2))
plot(Divorce ~ Marriage, data=dat, pch=21, bg="gray", bty="l",
     xlab="Marriage rate", ylab="Divorce rate")
plot(Divorce ~ MedianAgeMarriage, data=dat, pch=21, bg="gray", bty="l",
     xlab="Median age marriage", ylab="Divorce rate")
par(mfrow=c(1,1))

# standardize some variables
dat$D <- c(scale(dat$Divorce))
dat$M <- c(scale(dat$Marriage))
dat$A <- c(scale(dat$MedianAgeMarriage))

# SD of the MedianAgeMarriage variable
sd(dat$MedianAgeMarriage)

# define the regression model (predicting the standardized divorce rate D from
# the standardized median age at marriage A)
model <- alist(D ~ dnorm(mu, sigma),
               mu <- a + bA * A,
               a ~ dnorm(0, 0.2),
               bA ~ dnorm(0, 0.5),
               sigma ~ dexp(1))

# fit the model using the quadratic approximation approach
res <- quap(model, data=dat)
res

# sample 1000 values from the prior distributions
set.seed(10)
prior <- as.data.frame(extract.prior(res))
head(prior)

# plot 50 of the regression lines based on the sampled values
plot(NA, xlim=c(-2,2), ylim=c(-2,2), xlab="Median age marriage (std)",
     ylab="Divorce rate (std)", bty="l")
apply(prior[1:50,], 1, function(par) abline(par[1], par[2], lwd=1.2, col="gray30"))

# extract 1000 samples from the posterior distributions of the intercept,
# slope, and error standard deviation
set.seed(10)
post <- extract.samples(res, n=1000)

# compute the predicted value (i.e., the expected value of D) based on each of
# the sampled intercept and slope values obtained above from A is equal to -3
pred <- apply(post, 1, function(par) par[1] + par[2] * -3)
head(pred)

# using the link() function, we can do the same thing; in fact, we will do the
# same when A is equal to -3, -2.8, ..., 3.2 (note: we keep resetting the seed
# to obtain the exact same values above and from link())
set.seed(10)
A_seq <- seq(from=-3, to=3.2, by=0.2)
mu <- link(res, data=list(A=A_seq))

# double-check that the predicted values for A = -3 are the same as the ones
# we obtained above manually
head(mu[,1])

# compute percentile interval of mean
mu.mean <- apply(mu, 2, mean)
mu.pi   <- apply(mu, 2, PI, prob=0.95)

# plot it all
plot(D ~ A, data=dat, pch=21, bg="gray", xlab="Median age marriage (std)",
     ylab="Divorce rate (std)", bty="l")
lines(A_seq, mu.mean, lwd=2)
shade(mu.pi, A_seq)

# show the x- and y-values on the original scale
plot(D ~ A, data=dat, pch=21, bg="gray", xlab="Median age marriage (std)",
     ylab="Divorce rate (std)", bty="l", xaxt="n", yaxt="n")
axis(side=1, at=((23:30) - mean(dat$MedianAgeMarriage)) / sd(dat$MedianAgeMarriage), labels=23:30)
axis(side=2, at=((6:14) - mean(dat$Divorce)) / sd(dat$Divorce), labels=6:14)
lines(A_seq, mu.mean, lwd=2)
shade(mu.pi, A_seq)

# define the regression model (predicting the standardized divorce rate D from
# the standardized marriage rate M)
model <- alist(D ~ dnorm(mu, sigma),
               mu <- a + bA * M,
               a ~ dnorm(0, 0.2),
               bA ~ dnorm(0, 0.5),
               sigma ~ dexp(1))

# fit the model using the quadratic approximation approach
res <- quap(model, data=dat)
res

## 5.1.1: Think before you regress

# install the dagitty package (if necessary)
#install.packages("dagitty")

# load the dagitty package
library(dagitty)

dag5.1 <- dagitty( "dag{ A -> D; A -> M; M -> D }" ) coordinates(dag5.1) <- list( x=c(A=0,D=1,M=2) , y=c(A=0,D=1,M=0) ) drawdag( dag5.1 )



