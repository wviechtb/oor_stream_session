############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-05-15
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 13.4 - ?
#
# last updated: 2025-05-15

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### 13.4: Latent-data formulation

# see Wikipedia for some details about the logistic distribution:
# https://en.wikipedia.org/wiki/Logistic_distribution
#
# in particular, note that the variance of a random variable following a
# logistic distribution is equal to s^2 * pi^2 / 3, where s is the 'scale'
# parameter

# Figure 13.5: plot of the pdf from a logistic distribution
xs <- seq(-6, 6, length.out=1000)
ys <- dlogis(xs, location=0, scale=1)
plot(xs, ys, type="l", lwd=5, bty="l", xlab="", ylab="")

# let's add to this plot the pdf from a normal distribution with the same mean
# and the same standard deviation
ys <- dnorm(xs, mean=0, sd=pi/sqrt(3))
lines(xs, ys, lwd=5, col="red")

# let's redraw the figure and shade the region from -Inf to 1
xs <- seq(-6, 6, length.out=1000)
ys <- dlogis(xs, location=0, scale=1)
plot(xs, ys, type="l", lwd=5, bty="l", xlab="", ylab="")
ys <- ys[xs <= 1]
xs <- xs[xs <= 1]
polygon(c(xs,rev(xs)), c(ys,rep(0,length(xs))), col="gray80", border=NA)
lines(xs, ys, lwd=5)

# the size of the shaded region corresponds to the probability of seeing a
# value between -Inf and 1 under such a logistic distribution; we can compute
# this probability via simulation
z <- rlogis(10^7, location=0, scale=1)
mean(z < 1)

# but we can compute this exactly using plogis()
plogis(1)

# logit^-1(x) = exp(x) / (1 + exp(x))
exp(1) / (1 + exp(1))

# Figure 13.6: pdf for a logistic distribution with mean (location) equal to
# -1.40 + 0.33 * 1 = -1.07 with the region > 0 shaded
xs <- seq(-6, 6, length.out=1000)
ys <- dlogis(xs, location=-1.07, scale=1)
plot(xs, ys, type="l", lwd=5, bty="l", xlab="", ylab="")
ys <- ys[xs > 0]
xs <- xs[xs > 0]
polygon(c(xs,rev(xs)), c(ys,rep(0,length(xs))), col="gray80", border=NA)
lines(xs, ys, lwd=5)

# the shaded area corresponds to the probability that y=1
plogis(0, location=-1.07, scale=1, lower.tail=FALSE)

## Interpretation of the latent variables

# what would happen if -1.40 + 0.33 * x is equal to 0? then there is a 50%
# probability of y=1 (and analogously, a 50% probability of y=0)
plogis(0, location=0, scale=1, lower.tail=FALSE)

# so -1.40 + 0.33 * x can be thought of as the 'propensity' of voting
# Republican, where 0 means an equal preference for the Republican versus
# Democrat candidate and values greater 0 indicate a higher preference for the
# Republican candidate

## Nonidentifiability of the latent scale parameter

# Figure 13.5: plot of the pdf from a logistic distribution
xs <- seq(-6, 6, length.out=1000)
ys <- dlogis(xs, location=0, scale=1)
plot(xs, ys, type="l", lwd=5, bty="l", xlab="", ylab="")

# now let's add to this plot the pdf from a normal distribution with the same
# mean but a standard deviation of 1.6; the two distributions are very similar
ys <- dnorm(xs, mean=0, sd=1.6)
lines(xs, ys, lwd=5, col="red")

############################################################################

### 13.5: Maximum likelihood and Bayesian inference for logistic regression

## Maximum likelihood using iteratively weighted least squares

# first think of y as a variable that follows a Bernoulli distribution (see
# Wikipedia: https://en.wikipedia.org/wiki/Bernoulli_distribution), where p is
# the probability that y=1; we can write the probability mass function (pmf)
# of such a variable as p^y * (1-p)^(1-y); so when y=1, then this is equal to
# p and when y=0, then this is equal to 1-p
#
# in a logistic regression model, p = logit^{-1}(X beta) (see equation 13.2),
# so we can write the pmf of y under such a model as:
#
# logit^{-1}(X beta)^y * (1-logit^{-1}(X beta))^(1-y)
#
# and since we have n subjects, we just take the product of these values (see
# equation 13.7) to get the joint pmf of (y_1, y_2, ..., y_n)
#
# however, in equation 13.7, the y values are observed/known, but what we do
# not know are the beta values; so when the data are fixed, but the beta
# values are unknown, then this makes equation 13.7 the likelihood of beta
# given the data

# let's try finding the value of beta (i.e., the intercept and slope) that
# maximizes the likelihood for the vote data from last time

# download the dataset if it doesn't already exist
if (!file.exists("nes.txt")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/NES/data/nes.txt", destfile="nes.txt")

# read in the dataset
dat <- read.table("nes.txt", header=TRUE)

# inspect the first six rows of the dataset
head(dat)

# keep only the data from 1992 and exclude respondents who preferred other candidates or had no opinion
ok <- dat$year==1992 & !is.na(dat$rvote) & !is.na(dat$dvote) & (dat$rvote==1 | dat$dvote==1)
dat <- dat[ok,]

# function that computes the log likelihood for a given beta vector and given x and y variables

fitfun <- function(beta, x, y) {

   p <- plogis(beta[1] + beta[2] * x)

   # we want to find the value of beta that maximizes the following
   #
   # prod(p^y * (1-p)^(1-y))
   #
   # however, the product of many probabilities becomes very small and
   # essentially indistinguishable from 0, which causes some numeric problems
   # when we use an algorithm to maximize the likelihood
   #
   # instead, we will find the value of beta that maximizes the log
   # likelihood, which is simply the following
   #
   # log(prod(p^y * (1-p)^(1-y)))
   #
   # which we can rewite as:
   #
   # = sum(log(p^y * (1-p)^(1-y)))
   # = sum(log(p^y) + log((1-p)^(1-y)))
   # = sum(y*log(p) + (1-y)*log(1-p))
   # = sum(y*log(p)) + sum((1-y)*log(1-p))

   loglikelihood <- sum(y * log(p)) + sum((1-y)*log(1-p))
   return(loglikelihood)

}

# find the value of beta that maximizes the function above
res <- optim(par=c(0,0), fitfun, x=dat$income, y=dat$rvote, control=list(fnscale=-1))
res

# res$par gives the estimated value of beta; res$value is the log likelihood

# fit the logistic regression model with maximum likelihood estimation using glm()
res <- glm(rvote ~ income, data=dat, family=binomial(link="logit"))
summary(res)
logLik(res)

## Bayesian inference with a uniform prior distribution

# fit the logistic regression model predicting rvote from income using
# uniform/flat priors on the intercept and slope
res <- stan_glm(rvote ~ income, family=binomial(link="logit"), data=dat,
                refresh=0, prior=NULL, prior_intercept=NULL)
print(res, digits=3)

# extract the samples from the posterior distributions of the intercept and slope parameters
post <- as.data.frame(res)

# use kernel density estimation to find the mode of the posterior distributions
den <- density(post[,1], n=4096)
plot(den, main="Kernel density estimate of the posterior distribution for the intercept",
     bty="l", lwd=5)
den$x[which.max(den$y)]
den <- density(post[,2], n=4096)
plot(den, main="Kernel density estimate of the posterior distribution for the slope",
     bty="l", lwd=5)
den$x[which.max(den$y)]

# fit the logistic regression model using the default (weakly informative) priors
res <- stan_glm(rvote ~ income, family=binomial(link="logit"), data=dat, refresh=0)
print(res, digits=3)

## Bayesian inference with some prior information

# fit the logistic regression model using a slightly more informative N(0,1)
# prior on the slope
res <- stan_glm(rvote ~ income, family=binomial(link="logit"), data=dat, refresh=0,
                prior=normal(0,1))
print(res, digits=3)
