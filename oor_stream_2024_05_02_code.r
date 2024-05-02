############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-05-02
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 8.1 - ?
#
# last updated: 2024-05-02

############################################################################

# for the survey about topics for future streams (alternating with the coverage
# of the present book), go here: https://forms.gle/VEEKNRvquRCGKjb78

############################################################################

# - version R 4.4-0 released a week ago
# - security vulnerability in older version of R
#   - https://hiddenlayer.com/research/r-bitrary-code-execution/
#   - https://www.kb.cert.org/vuls/id/238194
#   - https://nvd.nist.gov/vuln/detail/CVE-2024-27322
#   - https://stat.ethz.ch/pipermail/r-help/2024-May/479281.html
# - sort_by() function

############################################################################

### 8.1: Least squares, maximum likelihood, and Bayesian inference

## Least squares

# simulate some data based on a simple regression model
set.seed(1239)
n <- 50
x <- runif(n, 0, 10)
y <- 2 + 0.5 * x + rnorm(n, mean=0, sd=1)

# plot the data
plot(x, y, pch=21, bg="gray")

# add the true regression line to the plot
abline(a=2, b=0.5, lwd=3)

# since we know the true regression line here, we can compute the errors
y - (2 + 0.5 * x)

# create the X and y matrices
X <- cbind(1, x)
X
y <- cbind(y)
y

# the regression model can then be written in matrix notation as:
#
# y = X %*% beta + e
#
# where beta is a column vector with the true intercept and slope

# now apply equation (8.2) to compute the estimated intercept and slope
betahat <- solve(t(X) %*% X) %*% t(X) %*% y
betahat

# change y back to a vector
y <- c(y)

# double-check that this gives the same estimates as (8.3) and (8.4)
b <- sum((x - mean(x)) * y) / sum((x - mean(x))^2)
a <- mean(y) - b * mean(x)
rbind(a, b)

# add the estimated regression line to the plot
abline(a, b, lwd=3, lty="dotted")

# fit the model using lm()
res <- lm(y ~ x)
summary(res)

# aside from being tedious, it should be noted that the 'manual' computations
# above are not how the estimates of the intercept and slope should be computed;
# lm() internally uses equations that are numerically more stable

# compute the residuals
resid <- y - (a + b * x)
resid

# check that the mean of the residuals is zero (note: due to numerical
# imprecision, this value is not exactly equal to 0, but practically
# indistinguishable from zero)
mean(resid)

# compute the RSS
sum(resid^2)

# you cannot make this any smaller with other values of 'a' and 'b'

# function to compute the RSS for given values of 'a' and 'b'
rss <- function(x, y, a, b) {
   resid <- y - (a + b*x)
   return(sum(resid^2))
}

# double-check that we get the same RSS as above
rss(x, y, a, b)

# try a few other values for 'a' and 'b'
rss(x, y, a=2.3, b=0.41)
rss(x, y, a=2.1, b=0.41)
rss(x, y, a=2.1, b=0.40)

## Estimation of residual standard deviation sigma

# we know that the true sigma is equal to 1 (since we simulated the data), but
# in practice, we would not know this; instead, we can estimate sigma based on
# the residuals; we can do this by computing the square root of 1/n * RSS
sqrt(1/n * sum(resid^2))

# but a better estimate is to divide by n-2 (where the 2 is the number of
# regression coefficients of the model)
sqrt(1/(n-2) * sum(resid^2))

# re-write the function so that it takes a vector with the intercept and slope
# as input
rss <- function(par, x, y) {
   a <- par[1]
   b <- par[2]
   resid <- y - (a + b*x)
   rss <- sum(resid^2)
   cat("a =", formatC(a, format="f", flag=" ", digits=6),
       "b =", formatC(b, format="f", flag=" ", digits=6),
       "rss =", formatC(rss, format="f", digits=6), "\n")
   return(rss)
}

# now use numerical optimization to iteratively find the intercept and slope
# values that minimize the RSS
optim(c(0,0), rss, x=x, y=y)

# we essentially get the same estimates (although there are minor numerical
# differences that arise because of the use of an iterative procedure for
# finding the estimates)

## Maximum likelihood

# if we set the intercept, slope, and sigma value, we can compute the density of
# the observed data as follows; say we assume that the intercept is 2.2, the
# slope is 0.4, and sigma is 1.2, then we get the following density values for
# the data under a normal distribution
p <- dnorm(y, mean = 2.2 + 0.4 * x, sd = 1.2)
p

# we can multiply these values to get the joint density
prod(p)

# but since the parameters (i.e., the intercept, slope, and sigma) are unknown,
# we call this the likelihood of the parameters given the data and we want to
# find those parameter values (estimates) that are most likely given the data;
# those are the maximum likelihood estimates

# for numerical reasons, instead of maximizing the product of the density
# values, we will maximize the sum of the log-transformed values
sum(log(p))

# this is the log likelihood given the parameter estimates we assumed

# function that computes the log likelihood
mle <- function(par, x, y) {
   a <- par[1]
   b <- par[2]
   sigma <- par[3]
   logp <- dnorm(y, mean = a + b * x, sd = sigma, log=TRUE)
   ll <- sum(logp)
   cat("a =", formatC(a, format="f", flag=" ", digits=6),
       "b =", formatC(b, format="f", flag=" ", digits=6),
       "sigma =", formatC(sigma, format="f", flag=" ", digits=6),
       "ll =", formatC(ll, format="f", digits=6), "\n")
   return(ll)
}

# again use numerical optimization to iteratively find the intercept, slope, and
# sigma values that maximize the log likelihood (note: optim() does minimization
# by default, so we have to tell it to maximize)
optim(c(0,0,2), mle, x=x, y=y, control=list(fnscale=-1))

# again for minor numerical differences, the MLEs are identical to the least
# squares estimates, except for sigma, where the MLE is identical to
sqrt(1/n * sum(resid^2))

## Where do the standard errors come from? Using the likelihood surface to
## assess uncertainty in the parameter estimates

# re-write the mle() function so that sigma is directly computed from the
# intercept and slope parameters and that it optionally either returns the
# likelihood or the log likelihood (with the former being the default)
mle <- function(par, xvals, yvals, log=FALSE) {
   a <- par[1]
   b <- par[2]
   n <- length(yvals)
   sigma <- sqrt(1/n * sum((yvals - (a + b*xvals))^2)) # MLE of sigma
   if (log) {
      logp <- dnorm(yvals, mean = a + b * xvals, sd = sigma, log=TRUE)
      ll <- sum(logp)
      return(ll)
   } else {
      p <- dnorm(yvals, mean = a + b * xvals, sd = sigma)
      l <- prod(p)
      return(l)
   }
}

# compute the likelihood for various combinations of intercept and slope values

as <- seq(1.4, 3.2, length=100)
bs <- seq(0.2, 0.7, length=100)
ll <- matrix(NA, nrow=length(as), ncol=length(bs))

for (i in 1:length(as)) {
   for (j in 1:length(bs)) {
      ll[i,j] <- mle(c(as[i], bs[j]), xvals=x, yvals=y)
   }
}

# create a perspective plot of the likelihood surface (like Figure 8.1(a),
# except that we are using the simulated data from above)
persp(as, bs, ll)

# extract the variance-covariance matrix of the parameter estimates from the
# regression model that we fitted earlier using lm()
vcoef <- vcov(res)
vcoef

# the values along the diagonal of this matrix are the squared standard errors;
# so the square root of the diagonal elements are the standard errors
se <- sqrt(diag(vcoef))
se

# turn the variance-covariance matrix into a correlation matrix; we see that the
# estimate of the intercept and slope are negatively correlated
cov2cor(vcoef)

# install the ellipse package
#install.packages("ellipse")

# load the ellipse package
library(ellipse)

# instead of a perspective plot, we can visualize the surface using a contour
# plot with colors indicating the height; we also indicate the peak with a red
# dot and lines extending from that dot +- one standard error for each
# coefficient (recall that this encompasses 68% of the distribution of a normal
# distribution); we also add the contour ellipse that encompasses 68% of the
# joint distribution of the two coefficients assuming bivariate normality
filled.contour(as, bs, ll, color.palette=hcl.colors,
               xlab="Intercept", ylab="Slope",
               plot.axes = {
                  axis(side=1)
                  axis(side=2)
                  xy <- ellipse(vcoef, centre=c(a,b), level=0.68)
                  lines(xy[,1], xy[,2], lwd=3, col="red")
                  segments(a-se[1], b, a+se[1], b, lwd=3, col="red")
                  segments(a, b-se[2], a, b+se[2], lwd=3, col="red")
                  points(a, b, pch=19, col="red")
               })

# if we move away from the peak (red dot), then the drop in the likelihood is
# not so severe if an increase in the intercept value is paired with a decrease
# in the slope value (and vice-versa); this is due to the negative correlation
# between these two estimates

# install the numDeriv package
#install.packages("numDeriv")

# load the numDeriv package
library(numDeriv)

H <- hessian(mle, x=c(a,b), xvals=x, yvals=y, log=TRUE)
solve(-H)
vcoef

############################################################################


# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat", destfile="hibbs.dat")

# read in the data and inspect it
dat <- read.table("hibbs.dat", header=TRUE)
dat

############################################################################
