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
res <- lm(y ~ x, data=dat)
abline(res, lwd=5)
pred <- predict(res)
resid <- dat$y - pred
round(1 - var(resid) / var(dat$y), digits=2)

# multiply x and y by some constant; R^2 does not change
dat$x <- dat$x * 4
dat$y <- dat$y * 0.5
res <- lm(y ~ x, data=dat)
pred <- predict(res)
resid <- dat$y - pred
round(1 - var(resid) / var(dat$y), digits=2)

# fit the model with least squares and get R^2 and compute the squared
# correlation between x and y
res <- lm(y ~ x, data=dat)
pred <- predict(res)
resid <- dat$y - pred
1 - var(resid) / var(dat$y)
cor(dat$x, dat$y)^2

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
1 - var(resid) / var(dat$y)
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
pred <- predict(res1)
resid <- dat1$logweight - pred
R2all <- 1 - var(resid) / var(dat1$logweight)
R2all

# fit the model to dat1, but restricted to data where height is between 65 and 70
sub <- subset(dat1, height >= 65 & height <= 70)
res3 <- lm(logweight ~ height, data=sub)

# compute R^2 for this model
pred <- predict(res3)
resid <- sub$logweight - pred
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

# Figure 11.17: plot of the toy dataset
dat <- data.frame(x = 1:5 - 3, y = c(1.7, 2.6, 2.5, 4.4, 3.8) - 3)
plot(y ~ x, data=dat, pch=19, xlim=c(-2,2), ylim=c(-2,2), cex=0.8, bty="l")

# add the least squared fit
res1 <- lm(y ~ x, data=dat)
abline(res1, lwd=3, col="dodgerblue")

# compute R^2
pred <- predict(res1)
resid <- dat$y - pred
round(1 - var(resid) / var(dat$y), digits=2)

# load the rstanarm package
library(rstanarm)

# Bayes fit with strong priors as described in the book
res2 <- stan_glm(y ~ x, data=dat, refresh=0,
                 prior_intercept=normal(0, 0.2),
                 prior=normal(1, 0.2))
res2

# add the prior regression line to the plot
abline(a=0, b=1, lty="dashed", lwd=3)

# add the regression line based on the Bayes fit
abline(res2, lwd=3, col="firebrick")

# add a legend
legend("topleft", inset=0.02, lty=c("solid","solid","dashed"),
       col=c("dodgerblue","firebrick","black"), lwd=3, bty="n",
       legend=c("Least-squares fit", "Posterior mean fit", "Prior regression line"))

# note: the fitted line from the Bayesian model is actually based on the
# *median* value of the intercept and slope, so calling it the posterior mean
# fit is maybe not 100% accurate, but the difference between the mean and
# median will be very small here

# compute the predicted values and the residuals (note: the predicted values
# are computed based on the median intercept and median slope)
pred <- predict(res2)
resid <- dat$y - pred

# compute R^2 based on equation (11.1)
round(1 - var(resid) / var(dat$y), digits=2)

# we get the same value when using equation (11.2)
round(var(pred) / var(dat$y), digits=2)

# equation (11.2) gives a value above 1, which is nonsense; while (11.1) does
# give a reasonable value here, it ignores the uncertainty

# we can use the bayes_R2() function to get the Bayesian R^2 values as
# described in the book; note that we get 4000 of them, one for each sampled
# value from the posterior distributions of the intercept, slope, and sigma
head(bayes_R2(res2))

# compute the median Bayesian R^2 value
median(bayes_R2(res2))

# extract the sampled intercept, slope, and sigma values
sims <- as.data.frame(res2)
head(sims)

# for each intercept and slope pair, compute the predicted values
pred <- t(apply(sims, 1, function(b) b[1] + b[2] * dat$x))
pred[1:10,]

# we can do the same with posterior_epred()
pred <- posterior_epred(res2)
dimnames(pred) <- NULL
pred[1:10,]

# compute the Bayesian R^2 values based on equation (11.5)
R2 <- apply(pred, 1, var) / (apply(pred, 1, var) + sims$sigma^2)

# compare these values with those from bayes_R2() -- they are the same!
head(R2)
head(bayes_R2(res2))

# median, mean, and SD of the R^2 values (note: due to the randomness of the
# Bayesian fit, these differ somewhat from what is reported in the book)
round(median(R2), digits=2)
round(mean(R2), digits=2)
round(sd(R2), digits=2)

# now let's go back to the kidiq dataset

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# least-squares fit
res1 <- lm(kid_score ~ mom_iq + mom_hs, data=dat)
summary(res1)

# Bayesian fit
res2 <- stan_glm(kid_score ~ mom_iq + mom_hs, data=dat, refresh=0)
res2

# Figure 11.18: posterior distribution of the Bayesian R^2 values
hist(bayes_R2(res2), main="", breaks=30, xlab="Bayesian R^2", ylab="", yaxt="n")
abline(v=median(bayes_R2(res2)), lwd=3)

# construct a 95% percentile interval for the R^2 values
round(quantile(bayes_R2(res2), c(.025, .975)), digits=2)

# compare the calculation of R^2 from lm() with an equation that allows us to
# compute R^2 from the F-statistic of the model
summary(res1)$r.squared
Fval <- summary(res1)$fstatistic[[1]]
n <- nrow(dat)
p <- length(coef(res1))
Fval / (Fval + (n-p)/(p-1))

# however, when we assume that H0 is false, then Fval has a non-central
# F-distribution, which has a non-centrality parameter; using an iterative
# root finding search, we can determine the value of the non-centrality
# parameter so that an area of 0.025 of the F-distribution falls into the
# lower and upper tail
ncp.lo <- uniroot(function(ncp) pf(Fval, df1=2, df2=431, lower.tail=FALSE, ncp=ncp) - 0.025,
                  lower=1, upper=100)$root
ncp.hi <- uniroot(function(ncp) pf(Fval, df1=2, df2=431, lower.tail=TRUE, ncp=ncp) - 0.025,
                  lower=1, upper=1000)$root

# we can then convert these non-centrality parameter values into the bounds of
# a 95% confidence interval for R^2 (for details, see: Smithson, M. (2003).
# Confidence intervals. Sage. Section 4.3)
round(ncp.lo / (ncp.lo + p-1 + n-p + 1), digits=2)
round(ncp.hi / (ncp.hi + p-1 + n-p + 1), digits=2)

# this yields the same interval as we obtained earlier using the Bayesian
# approach

ncps <- seq(50, 200, length=1000)
denFval <- sapply(ncps, function(ncp) df(Fval, df1=2, df2=431, ncp=ncp))

hist(bayes_R2(res2), main="", breaks=30, xlab="Bayesian R^2", ylab="", yaxt="n", freq=FALSE)
lines(ncps / (ncps + p-1 + n-p + 1), denFval, lwd=5)
