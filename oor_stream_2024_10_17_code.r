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
hist(test_rep, xlim=range(min(dat$y), test_rep), breaks=40, main="", xlab="")
abline(v=min(dat$y), lwd=5)

