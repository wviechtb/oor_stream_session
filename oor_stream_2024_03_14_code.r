############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-03-14
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 5.4 - 5.6
#
# last updated: 2024-03-14

############################################################################

### 5.1: Bootstrapping to simulate a sampling distribution

# before considering the example given in the book, let's consider a simpler
# examples where we know the actual distribution of the statistic that we are
# interested in

# for example, say we simulate data from a normal distribution with true mean
# equal to 100 and true SD equal to 15
n <- 20
x <- rnorm(n, mean=100, sd=15)

# we can then compute the observed mean
mean(x)

# and we know based on theory that the standard error of the mean is given by
# the true SD divided by the square root of the sample size
15 / sqrt(n)

# let's confirm this via simulation

means <- replicate(100000, {
   x <- rnorm(n, mean=100, sd=15)
   mean(x)
})

hist(means, breaks=100)
abline(v=100, lwd=5)

# this is approximately equal to the true standard error; it only differs from
# the one we computed above because we only simulated 100,000 means
sd(means)

# but in practice, we only have a single sample
x <- rnorm(n, mean=100, sd=15)
x

# for which we can compute the mean
mean(x)

# since we do not know the true SD, we can still estimate the standard error
# of the mean by dividing the observed SD by the square root of the sample size
sd(x) / sqrt(n)

# now let's pretend we know nothing about statistical theory and we do not
# know this equation for computing (or more precisely: estimating) the
# standard error of the mean; we can use bootstrapping to obtain an estimate
# of the standard error of the mean

# take a sample of the same size as our data with replacement
x.boot <- sample(x, size=n, replace=TRUE)
x.boot

# compute the statistic of interest in the bootstrap sample
mean(x.boot)

# in bootstrapping, we repeat this process many times

means <- replicate(100000, {
   x <- sample(x, size=n, replace=TRUE)
   mean(x)
})

# inspect the bootstrap distribution of our statistic of interest
hist(means, breaks=100)

# note that it is *not* centered around the true mean, but it is centered
# around the observed mean; so we cannot use this distribution to magically
# recover what the true mean is; however, we can use the standard deviation of
# the bootstrap means to get an estimate of the standard error of our observed
# mean
sd(means)

# this is quite close to the standard error we computed above based on the
# theoretical equation

# now let's do a little simulation study to see how similar the standard error
# is when computed based on the theoretical equation versus bootstrapping

iters <- 1000
se.thry <- numeric(iters)
se.boot <- numeric(iters)

pbar <- txtProgressBar(min=0, max=iters, style=3)

for (s in 1:iters) {

   setTxtProgressBar(pbar, s)

   x <- rnorm(n, mean=100, sd=15)
   se.thry[s] <- sd(x) / sqrt(n)

   means <- replicate(1000, {
      x <- sample(x, size=n, replace=TRUE)
      mean(x)
   })
   se.boot[s] <- sd(means)

}

# scatterplot of the SE computed based on the theoretical equation versus the
# SE computed based on bootstrapping
plot(se.thry, se.boot, pch=19, cex=0.5, xlab="SE Based on Theory",
     ylab="SE Based on Bootstrapping")
abline(a=0, b=1, lwd=6, col="darkgray")
points(se.thry, se.boot, pch=19, cex=0.5)

# also add a horizontal and vertical line at the true SE
abline(h=15/sqrt(n), lty="dotted")
abline(v=15/sqrt(n), lty="dotted")

# so we can see that the SE based on bootstrapping is a quite close
# approximation to the SE based on theory (although if we look closely, we see
# that the bootstrap SEs tend to slightly underestimate the theoretical SEs)
mean(se.thry)
mean(se.boot)

# and we can compare the two above with the true SE
15 / sqrt(n)

# now let's go to the example from the book

# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv", destfile="earnings.csv")

# read in the dataset
dat <- read.csv("earnings.csv")

# compute the ratio of the median earnings of women versus the median earnings of men
with(dat, median(earn[male==0]) / median(earn[male==1]))

# since we do not know the theoretical equation for computing the standard
# error of the ratio of two medians, we will use bootstrapping to estimate it

n <- nrow(dat)

ratios <- replicate(10000, {
   dat.boot <- dat[sample(n, replace=TRUE),]
   with(dat.boot, median(earn[male==0]) / median(earn[male==1]))
})

sd(ratios)

# we can also examine the bootstrap distribution of the statistic of interest
# (since the earnings only take on a relatively small number of unique values,
# the ratio of the two medians can also only take on a relatively small number
# of unique values)
hist(ratios)

# let's do the bootstrapping using the boot package

# load the package
library(boot)

# function that takes the original data as input and a vector of indices that
# define the bootstrap sample and that then computes the statistic of interest
# based on the bootstrap sample
compratio <- function(x, idx) {
   dat.boot <- x[idx,]
   with(dat.boot, median(earn[male==0]) / median(earn[male==1]))
}

# run the bootstrapping
res <- boot(dat, statistic=compratio, R=10000)
res

# note: element 't' of res contains the bootstrap values of the statistic of
# interest (as a one column matrix), so we could also just compute the SD of
# these values ourselves
sd(res$t)

# note: using boot.ci(res), we can then obtain various types of confidence
# intervals for the true ratio, but this is now really beyond the discussion
# in the book, so let's skip this

############################################################################

### 6.2: Fitting a simple regression to fake data

# install the rstanarm package (need to do this once)
#install.packages("rstanarm")

# load the rstanarm package
library(rstanarm)

# simulate data as described in the book
x <- 1:20
n <- length(x)
a <- 0.2
b <- 0.3
sigma <- 0.5
set.seed(2141)
y <- a + b*x + rnorm(n, mean=0, sd=sigma)

# plot x versus y
plot(x, y, pch=21, bg="gray")

# create a data frame with x and y
dat <- data.frame(x, y)

# remove the x and y vector objects from the workspace
rm(x, y)

# fit a simple linear regression model using stan_glm()
res <- stan_glm(y ~ x, data=dat)
print(res, digits=2)

############################################################################

