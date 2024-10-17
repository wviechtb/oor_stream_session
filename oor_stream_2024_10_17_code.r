############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-10-17
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 11.4 - ?
#
# last updated: 2024-10-17

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### addendum to the last section

# the book doesn't actually give examples of what a 'fitted versus residuals'
# plot might look like when some of the model assumptions are violated; let's
# consider a few examples

# simulate data with a non-linear relationship between x and y
set.seed(1234)
N <- 500
x <- runif(N, 0, 1)
y <- 1 + 4*x + -2*x^2 + rnorm(N, mean=0, sd=0.25)
dat <- data.frame(x=x, y=y)
head(dat)

# plot x versus y
plot(dat$x, dat$y, xlab="x", ylab="y", pch=21, bg="darkgray")

# fit the model that assumes a linear relationship between x and y
res <- stan_glm(y ~ x, data=dat, refresh=0)
res

# compute the predicted values and residuals
pred  <- predict(res)
resid <- dat$y - pred

# plot the fitted values versus the residuals
plot(pred, resid, pch=19, main="Residuals vs. predicted values",
     xlab="predicted value", ylab="residual")
abline(h=0, col="gray", lwd=5)

# we see an upside down U shape in the plot, which is indicative of missing a
# non-linear relationship

# simulate data with heteroscedasticity
set.seed(1234)
N <- 500
x <- runif(N, 0, 1)
y <- 1 + 1*x + rnorm(N, mean=0, sd=(0.1 + 0.3*(1 - x)))
dat <- data.frame(x=x, y=y)
head(dat)

# plot x versus y
plot(dat$x, dat$y, xlab="x", ylab="y", pch=21, bg="darkgray")

# fit the model that assumes homoscedasticity
res <- stan_glm(y ~ x, data=dat, refresh=0)
res

# compute the predicted values and residuals
pred  <- predict(res)
resid <- dat$y - pred

# plot the fitted values versus the residuals
plot(pred, resid, pch=19, main="Residuals vs. predicted values",
     xlab="predicted value", ylab="residual")
abline(h=0, col="gray", lwd=5)

# we see that the residuals fluctuate more for low predicted values, which is
# indicative of heteroscedasticity

############################################################################

### 11.4: Comparing data to replications from a fitted model

# clean up the workspace
rm(list=ls())

## Example: simulation-based checking of a fitted normal distribution

# download the dataset (need to do this once)
if (!file.exists("newcomb.txt")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/Newcomb/data/newcomb.txt", destfile="newcomb.txt")

# read in the data and inspect the first 6 rows
dat <- read.table("newcomb.txt", header=TRUE)
head(dat)

# Figure 11.9: histogram of the y values in the dataset
hist(dat$y, main="", breaks=40)

# fit a linear regression model without any predictors
res <- stan_glm(y ~ 1, data=dat, refresh=0)

# extracted the sampled values from the posterior distribution of the
# intercept and error standard deviation
sims <- as.data.frame(res)
head(sims)

# simulate new data based on the sampled intercept and error SD values
set.seed(1234)
y_rep <- apply(sims, 1, function(x) rnorm(nrow(dat), mean=x[1], sd=x[2]))

# so we get 4000 simulated datasets
dim(y_rep)

# let's transpose y_rep, so each row corresponds to a dataset
y_rep <- t(y_rep)

# examine the first 6 data points of datasets 1 through 5
y_rep[1:5,1:6]

# we can also use posterior_predict() to accomplish the same thing
set.seed(1234)
y_rep <- posterior_predict(res)
y_rep[1:5,1:6]

# Visual comparison of actual and replicated datasets

# Figure 11.10: histogram of 20 randomly selected datasets from y_rep
par(mfrow=c(5,4), mar=c(3,3,2,2))
for (s in sample(nrow(y_rep), 20)){
   hist(y_rep[s,], main="", xlab="", ylab="")
}
par(mfrow=c(1,1), mar=c(5,4,4,2))

# Figure 11.11: plot the kernel density estimate of the distribution of y
plot(density(dat$y), lwd=5, main="", xlim=c(-50,55))

# add the kernel density estimate of each simulated dataset to the plot
apply(y_rep, 1, function(x) lines(density(x), col=rgb(0,0,0,.02)))

# Checking model fit using a numerical data summary

# compute the minimum value for each simulated dataset
test_rep <- apply(y_rep, 1, min)

# Figure 11.12: histogram of these minimum values with a vertical line at the
# minimum value observed in the actual data
hist(test_rep, xlim=c(-50,20), breaks=40, main="", xlab="")
abline(v=min(dat$y), lwd=5)

############################################################################

# before we get to the type of model discussed in the book, we will first do
# another illustration of the principle discussed at the end of section 11.4
# using the dataset from section 11.5

# download the dataset (need to do this once)
if (!file.exists("unemp.txt")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/Unemployment/data/unemp.txt", destfile="unemp.txt")

# read in the data and inspect the first 6 rows
dat <- read.table("unemp.txt", header=TRUE)
head(dat)

# Figure 11:13: unemployment rate over time
plot(dat$year, dat$y, type="l", xlab="Year", ylab="Unemployment rate",
     ylim=c(0,10), bty="l", lwd=3)

# say we want to model the trend in the unemployment rate as a simple linear
# model with year as the predictor
res <- stan_glm(y ~ year, data=dat, refresh=0)
print(res, digits=3)

# extract the posterior samples
sims <- as.data.frame(res)
head(sims)

# based on the model, we can be 99.9+% certain that the slope is positive
mean(sims$year > 0)

# but maybe our model is wrong! let's put some of the ideas we have learned so
# far in this chapter into practice

# compute the predicted values and residuals
pred  <- predict(res)
resid <- dat$y - pred

# plot the fitted values versus the residuals
plot(pred, resid, pch=19, main="Residuals vs. predicted values",
     xlab="predicted value", ylab="residual")
abline(h=0, col="gray", lwd=5)

# maybe there is a hint of an upside down U shape in the plot, but let's not
# overinterpret this

# simulate new data based on the model using posterior_predict()
y_rep <- posterior_predict(res)

# plot the observed data and add the lines for 10 randomly chosen simulated datasets
plot(dat$year, dat$y, type="l", xlab="Year", ylab="Unemployment rate",
     ylim=c(0,12), bty="l", lwd=3)
apply(y_rep[sample(nrow(y_rep), 10),], 1, function(x) lines(dat$year, x, col="gray70"))
lines(dat$year, dat$y, lwd=3)

# while of course the lines from the simulated datasets are different from the
# actually observed line, we cannot easily see here if something is amiss

# let's try plotting a kernel density estimate of the observed residuals
plot(density(resid), lwd=5, main="", xlim=c(-6,6), ylim=c(0,0.35))

# add the kernel density estimate of the residuals from each simulated dataset
# to the plot (we'll use a for-loop for this)
for (i in 1:nrow(y_rep)) {
   lines(density(y_rep[i,] - (sims[i,1] + dat$year*sims[i,2])), col=rgb(0,0,0,.02))
}

# we see that the distribution of the observed residuals is somewhat unusual
# compared to the distributions based on the simulated data; but remember that
# the normality assumption is not one of the more critical assumptions, so
# maybe this is not something to worry about that much

# but an assumption that is more important to worry about is the independent
# of the errors assumption; we will check this by computing the correlation
# between adjacent residuals (the lag-1 autocorrelation)
n <- length(resid)
ar1 <- cor(resid[1:(n-1)], resid[2:n])
ar1

# compute the lag-1 autocorrelations based on the simulated datasets
ar1i <- rep(NA, nrow(y_rep))
for (i in 1:nrow(y_rep)) {
   r_rep <- y_rep[i,] - (sims[i,1] + dat$year*sims[i,2])
   ar1i[i] <- cor(r_rep[1:(n-1)], r_rep[2:n])
}

# plot the kernel density estimate of these autocorrelations and add a
# vertical line at the actually observed autocorrelation
plot(density(ar1i), lwd=2, xlim=c(-0.5,0.8), main="")
abline(v=ar1, lwd=5)

# we see that the actually observed autocorrelation is much higher than those
# observed in the simulated datasets; this shows that our model is wrong (as
# it assumes that the errors are independent, which is apparently not true for
# these data)

############################################################################

### 11.5: Example: predictive simulation to check the fit of a time-series model

## Fitting a first-order autoregression to the unemployment series

# add the lagged value of y to the dataset
dat$y_lag <- c(NA, dat$y[1:(n-1)])
head(dat)

# compute the lag-1 autocorrelation in y (note: not the residuals)
cor(dat$y, dat$y_lag, use="complete.obs")

# fit a linear regression model predicting y from the value from the previous year
res <- stan_glm(y ~ y_lag, data=dat, refresh=0)
print(res, digits=2)

# note that the coefficient for y_lag from the model is actually an estimate
# of the autocorrelation in the y variable; earlier, we modeled the linear
# trend in y and computed the autocorrelation in the residuals (~ 0.74); if we
# add year as a predictor to the autocorrelation model, then we will find a
# very similar estimate
res <- stan_glm(y ~ y_lag + year, data=dat, refresh=0)
print(res, digits=2)

# remember that based on the model that ignored the autocorrelation, we were
# 99.9+% certain the time trend in the unemployment rate is positive; let's
# see how certain we can be about this when we account for autocorrelation
sims <- as.data.frame(res)
mean(sims$year > 0)

# this is around 75%, which is still large, but considerably lower than what
# we obtained earlier; this shows violation of the independence assumption can
# have a considerable impact on the findings
