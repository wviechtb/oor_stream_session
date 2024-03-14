############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-03-14
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 5.4 - ?
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


############################################################################
