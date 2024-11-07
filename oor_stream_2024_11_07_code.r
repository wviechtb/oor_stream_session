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

### 11.6: Residual standard deviation sigma and explained variance R^2

# download the dataset (need to do this once)
if (!file.exists("kidiq.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# fit a linear regression model predicting the kids' test score from the
# moms' IQ and whether the mom graduated from high-school or not
res <- lm(kid_score ~ mom_iq + mom_hs, data=dat)
summary(res)

# we see that sigma is estimated to be around 18

# compute the predicted values and the residuals
pred <- predict(res)
resid <- dat$kid_score - pred

# we can think of sigma as the average distance of the observations from the
# corresponding predicted values, but this is just a rough approximation
mean(abs(resid))

# really, sigma is the square-root of the average squared distance
sqrt(mean(resid^2))

# but the latter is of course more difficult to think about

# compute R^2 based on equation (11.1)
round(1 - var(resid) / var(dat$kid_score), digits=2)

# we get the same value when using equation (11.2)
round(var(pred) / var(dat$kid_score), digits=2)

# simulate data for the model y = beta0 + beta1 * x + error when beta1 is
# around 0, fit the corresponding model, and calculate R^2

set.seed(1234)
n <- 1000
dat <- data.frame(x = runif(n, 0, 1))
dat$y <- 5 + 0.02 * dat$x + rnorm(n, mean=0, sd=0.5)
plot(y ~ x, data=dat, pch=19, cex=0.5)
res <- lm(y ~ x, data=dat)
abline(res, lwd=5)
pred <- predict(res)
resid <- dat$y - pred
round(1 - var(resid) / var(dat$y), digits=2)

# simulate data for the model y = beta0 + beta1 * x + error when beta1 is
# not 0 and sigma is very small, fit the corresponding model, and calculate R^2

set.seed(1234)
n <- 1000
dat <- data.frame(x = runif(n, 0, 1))
dat$y <- 5 + 0.1 * dat$x + rnorm(n, mean=0, sd=0.005)
plot(y ~ x, data=dat, pch=19, cex=0.5)
res <- lm(y ~ x, data=dat, refresh=0)
abline(res, lwd=5)
pred <- predict(res)
resid <- dat$y - pred
round(1 - var(resid) / var(dat$y), digits=2)

# multiply x and y by some constant; R^2 does not change
dat$x <- dat$x * 4
dat$y <- dat$y * 0.5
res <- lm(y ~ x, data=dat, refresh=0)
pred <- predict(res)
resid <- dat$y - pred
round(1 - var(resid) / var(dat$y), digits=2)

# fit the model with least squares and get R^2 and compute the squared
# correlation between x and y
res <- lm(y ~ x, data=dat)
pred <- predict(res)
resid <- dat$y - pred
1 - sigma(res)^2 / var(dat$y)
cor(dat$x, dat$y)^2

# note that these two are almost identical, but not exactly; but if we compute
# the variance of the observed data with n-2 in the denominator (which is also
# what is used for computing sigma), then the two are exactly identical
1 - sigma(res)^2 / (var(dat$y) * (n-1)/(n-2))

# and this also matches the R^2 value that lm() reports
summary(res)$r.squared

# there is actually a generalization of this idea when the model has more than
# one predictor; let's simulate some data for this, fit the model, and compute
# R^2 (note: here we use n-3 since the model has three coefficients)
dat <- data.frame(x1 = runif(n, 0, 1), x2 = rnorm(n, 0, 1))
dat$y <- 5 + 0.6 * dat$x1 + -0.2 * dat$x2 + rnorm(n, mean=0, sd=0.5)
res <- lm(y ~ x1 + x2, data=dat)
pred <- predict(res)
resid <- dat$y - pred
1 - sigma(res)^2 / (var(dat$y) * (n-1)/(n-3))
summary(res)$r.squared

# if x1 and x2 were perfectly uncorrelated, then the sum of the squared
# correlations of each predictor with the outcome would be identical to R^2
cor(dat$x1, dat$y)^2 + cor(dat$x2, dat$y)^2

# this actually comes quite close here, since x1 and x2 are almost uncorrelated
cor(dat$x1, dat$x2)

# however, often the predictors are much more correlated, in which case this
# doesn't work anymore; however, the squared correlation between the predicted
# values and the outcome is exactly identical to R^2
cor(pred, dat$y)^2

## Difficulties in interpreting residual standard deviation and explained variance

# Figure 11.15 (two simulated datasets for height and log(weight))

par(mfrow=c(1,2), mar=c(5,4,2,2))

dat1 <- data.frame(height = round(rnorm(n, 68, 3.2)))
dat1$logweight <- 2.5 + 0.04 * dat1$height + rnorm(n, mean=0, sd=0.17)
plot(logweight ~ jitter(height), data=dat1, pch=19, cex=0.4,
     xlim=c(58,81), ylim=c(4,6.3), xlab="height", ylab="log(weight)")
res1 <- lm(logweight ~ height, data=dat1)
abline(res1, lwd=5)
sigma(res1)
text(78, 4.2, bquote(hat(sigma)==.(round(sigma(res1),2))))

dat2 <- data.frame(height = round(rnorm(n, 68, 3.2)))
dat2$logweight <- 2.5 + 0.04 * dat2$height + rnorm(n, mean=0, sd=0.32)
plot(logweight ~ jitter(height), data=dat2, pch=19, cex=0.4,
     xlim=c(58,81), ylim=c(4,6.3), xlab="height", ylab="log(weight)")
res2 <- lm(logweight ~ height, data=dat2)
abline(res2, lwd=5)
sigma(res2)
text(78, 4.2, bquote(hat(sigma)==.(round(sigma(res2),2))))

par(mfrow=c(1,1))

# both models have the same 'deterministic part' but sigma differs greatly

# compute R^2 for dat1
pred <- predict(res)
resid <- dat1$y - pred
R2all <- 1 - sigma(res1)^2 / (var(dat1$logweight) * (n-1)/(n-2))
R2all

# fit the model to dat1, but restricted to data where height is between 65 and 70
sub <- subset(dat1, height >= 65 & height <= 70)
res3 <- lm(logweight ~ height, data=sub)

# compute R^2 for this model
pred <- predict(res)
resid <- sub$y - pred
R2sub <- 1 - sigma(res3)^2 / (var(sub$logweight) * (n-1)/(n-2))
R2sub

# Figure 11.16

par(mfrow=c(1,2), mar=c(5,4,2,2))

plot(logweight ~ jitter(height), data=dat1, pch=19, cex=0.4,
     xlim=c(58,81), ylim=c(4,6.3), xlab="height", ylab="log(weight)")
abline(res1, lwd=5)
text(77, 6.2, bquote(R^2 ==.(round(R2all,2))))

plot(logweight ~ jitter(height), data=sub, pch=19, cex=0.4,
     xlim=c(58,81), ylim=c(4,6.3), xlab="height", ylab="log(weight)")
abline(res3, lwd=5)
text(77, 6.2, bquote(R^2 ==.(round(R2sub,2))))

par(mfrow=c(1,1))

## Bayesian R^2


# load the rstanarm package
library(rstanarm)

