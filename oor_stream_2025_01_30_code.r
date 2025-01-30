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

############################################################################

### 12.2: Centering and standardizing for models with interactions

# download the dataset if it doesn't already exist
if (!file.exists("kidiq.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# model with the interaction between mom_hs and mom_iq
res <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=dat, refresh=0)
res

## Centering by subtracting the mean of the data

# center each predictor at its mean
dat$c_mom_hs <- dat$mom_hs - mean(dat$mom_hs)
dat$c_mom_iq <- dat$mom_iq - mean(dat$mom_iq)

# refit the model with these centered predictors
res <- stan_glm(kid_score ~ c_mom_hs + c_mom_iq + c_mom_hs:c_mom_iq, data=dat, refresh=0)
res

## Using a conventional centering point

# center each predictor at a sensible reference point
dat$c2_mom_hs <- dat$mom_hs - 0.5
dat$c2_mom_iq <- dat$mom_iq - 100

# sidenote: mean(dat$mom_iq) is exactly 100 for this dataset, so the mom IQ
# scores were already rescaled to have exactly this mean (and sd(dat$mom_iq)
# is exactly 15)

# refit the model with these centered predictors
res <- stan_glm(kid_score ~ c2_mom_hs + c2_mom_iq + c2_mom_hs:c2_mom_iq, data=dat, refresh=0)
res

## Standardizing by subtracting the mean and dividing by 2 standard deviations

# standardize each predictor and divide by 2
dat$z_mom_hs <- (dat$mom_hs - mean(dat$mom_hs)) / (2*sd(dat$mom_hs))
dat$z_mom_iq <- (dat$mom_iq - mean(dat$mom_iq)) / (2*sd(dat$mom_iq))

# refit the model with these standardized predictors
res <- stan_glm(kid_score ~ z_mom_hs + z_mom_iq + z_mom_hs:z_mom_iq, data=dat, refresh=0)
res
