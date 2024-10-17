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

### 11.4: Comparing data to replications from a fitted model

