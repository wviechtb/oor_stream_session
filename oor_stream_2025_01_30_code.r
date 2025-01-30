############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-01-30
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 12.1 - ?
#
# last updated: 2025-01-30

############################################################################

# load the rstanarm package
library(rstanarm)

### 12.1: Linear transformations

## Scaling of predictors and regression coefficients

# download the dataset (only need to do this once)
if (!file.exists("earnings.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv", destfile="earnings.csv")

# read in the dataset
dat <- read.csv("earnings.csv")

# inspect the first six rows of the dataset
head(dat)

# fit a model predicting earnings from height
res <- stan_glm(earn ~ height, data=dat, refresh=0, seed=7783)
res

# Figure 12.1(a): plot of height versus weight (with some jittering on the height values)
plot(earn ~ jitter(height, amount=0.2), data=dat, pch=19, cex=0.3,
     xlab="height", bty="l")

# extract the sampled values for the posterior distributions
post <- as.data.frame(res)
head(post)

# add 10 lines based on these posterior samples
apply(post[1:10,], 1, function(b) abline(b[1], b[2]))

# Figure 12.1(b): same plot but the x-axis extended to 0
plot(earn ~ jitter(height, amount=0.2), data=dat, pch=19, cex=0.3,
     xlab="height", bty="l", xlim=c(0, max(dat$height)),
     ylim=c(coef(res)[1], max(dat$earn)))

# add 10 lines based on these posterior samples
apply(post[1:10,], 1, function(b) abline(b[1], b[2]))

# fit a model predicting earnings from height in millimeters
dat$height_mm <- dat$height * 25.4
res <- stan_glm(earn ~ height_mm, data=dat, refresh=0, seed=7783)
res

# fit a model predicting earnings from height in miles
dat$height_miles <- dat$height / 63360
res <- stan_glm(earn ~ height_miles, data=dat, refresh=0, seed=7783)
res

# fit a model predicting earnings from standard deviation units of height
dat$height_sd <- dat$height / sd(dat$height)
res <- stan_glm(earn ~ height_sd, data=dat, refresh=0, seed=7783)
res

## Standardization using z-scores

# fit a model predicting earnings from standardized height
dat$height_z <- c(scale(dat$height))
# same as dat$height_z <- (dat$height - mean(dat$height)) / sd(dat$height)
res <- stan_glm(earn ~ height_z, data=dat, refresh=0, seed=7783)
res

