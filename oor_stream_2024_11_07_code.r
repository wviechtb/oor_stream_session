############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-11-07
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 11.6 - ?
#
# last updated: 2024-11-07

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### 11.6: Residual standard deviation sigma and explained variance R^2

# download the dataset (need to do this once)
if (!file.exists("kidiq.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# fit a linear regression model predicting the kids' test score from the
# moms' IQ and whether the mom graduated from high-school or not
res <- stan_glm(kid_score ~ mom_iq + mom_hs, data=dat, refresh=0)
res

# we see that sigma is estimated to be around 18 (or more precisely, the
# median value of the sampled values of the posterior distribution of sigma is
# around 18)

# compute the predicted values (based on the median intercept and median slope
# values) and the residuals
pred <- predict(res)
resid <- dat$kid_score - pred

# we can think of sigma as the average distance of the observations from the
# corresponding predicted values, but this is just a rough approximation
mean(abs(resid))

# really, sigma is the square-root of the average squared distance
sqrt(mean(resid^2))

# but the latter is of course more difficult to think about

# compute R^2 based on equation (11.1)
round(1 - sigma(res)^2 / var(dat$kid_score), digits=2)

# we get essentially the same value when using equation (11.2)
var(pred) / var(dat$kid_score)

# this doesn't really have anything to do with whether we use least squares or
# not; in fact, even for least squares estimation, the two equations give
# slightly different answers
res <- lm(kid_score ~ mom_iq + mom_hs, data=dat)
pred <- predict(res)
resid <- dat$kid_score - pred
1 - sigma(res)^2 / var(dat$kid_score)
var(pred) / var(dat$kid_score)

# but the difference is really not relevant

# simulate data for the model y = beta0 + beta1 * x + error when beta1 is
# around 0, fit the corresponding model, and calculate R^2

set.seed(1234)
n <- 1000
dat <- data.frame(x = runif(n, 0, 1))
dat$y <- 5 + 0.002 * dat$x + rnorm(n, mean=0, sd=0.5)
plot(x, y, pch=19, cex=0.5)
res <- stan_glm(y ~ x, data=dat, refresh=0)
abline(res, lwd=5)
pred <- predict(res)
resid <- dat$y - pred
round(1 - sigma(res)^2 / var(dat$y), digits=2)

# simulate data for the model y = beta0 + beta1 * x + error when beta1 is
# not 0 and sigma is very small, fit the corresponding model, and calculate R^2

set.seed(1234)
n <- 1000
dat <- data.frame(x = runif(n, 0, 1))
dat$y <- 5 + 0.002 * dat$x + rnorm(n, mean=0, sd=0.5)
plot(x, y, pch=19, cex=0.5)
res <- stan_glm(y ~ x, data=dat, refresh=0)
abline(res, lwd=5)
pred <- predict(res)
resid <- dat$y - pred
round(1 - sigma(res)^2 / var(dat$y), digits=2)
