############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-09-28
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 4.2 - ?
#
# last updated: 2023-09-28

############################################################################

### 4.2: Estimates, standard errors, and confidence intervals

## Standard error for a comparison

# generate two vectors of data corresponding to the given example
x.m <- sample(c(rep(1,228), rep(0,172)))
x.m
x.w <- sample(c(rep(1,270), rep(0,330)))
x.w

# compute the observed proportions
prop.m <- mean(x.m)
prop.m
prop.w <- mean(x.w)
prop.w

# compute the (estimated) standard errors of these two proportions
se.m <- sqrt(prop.m * (1-prop.m) / length(x.m))
se.w <- sqrt(prop.w * (1-prop.w) / length(x.w))
se.m
se.w

# compute the difference between the two proportions
prop.m - prop.w

# compute the (estimated) standard error of this difference
sqrt(se.m^2 + se.w^2)

# so now we can construct an approximate 95% confidence interval for the
# true difference in the same manner as we did previously
(prop.m - prop.w) - 2*sqrt(se.m^2 + se.w^2)
(prop.m - prop.w) + 2*sqrt(se.m^2 + se.w^2)

## Sampling distribution of the sample mean and standard deviation;
## normal and chi^2 distributions

# let's repeat again what we essentially did last time and simulate 10
# observations from a normal distribution with true mean 175 and true standard
# deviation 10 and then compute the mean and standard deviation and then
# repeat this 100,000 times
set.seed(1234)
n <- 10
stats <- replicate(100000, {
   x <- rnorm(n, mean=175, sd=10)
   c(mean(x), sd(x))
})

# the first row of stats includes the means, so look at the corresponding
# sampling distribution and superimpose the theoretical one
hist(stats[1,], breaks=80, xlab="Mean",
     main="Sampling Distribution of the Mean", freq=FALSE)
curve(dnorm(x, mean=175, sd=10/sqrt(n)), add=TRUE, lwd=5)

# the second row includes the standard deviations, so look at the
# corresponding sampling distribution
hist(stats[2,], breaks=60, xlab="Standard Deviation",
     main="Sampling Distribution of the Standard Deviation", freq=FALSE)

# check that the distribution of the scaled variances is really a chi-squared
# distribution with n-1 degrees of freedom
hist(stats[2,]^2 * (n-1) / 10^2, breaks=60, xlab="Variance * (n-1) / sigma",
     main="Sampling Distribution of the Scaled Variance", freq=FALSE)
curve(dchisq(x, df=n-1), add=TRUE, lwd=5)

# from this, we can derive the distribution of the variance
hist(stats[2,]^2, breaks=80, xlab="Variance",
     main="Sampling Distribution of the Variance", freq=FALSE)
curve(dchisq(y * (n-1) / 10^2, df=n-1) * (n-1) / 10^2, add=TRUE, lwd=5, xname="y")

# https://online.stat.psu.edu/stat414/lesson/23/23.1
# x = sd^2 * (n-1) / sigma^2 ~ chi^2(df=n-1)
# y = x / (n-1) * sigma^2 (here y is then the variance)
# x = y * (n-1) / sigma^2
# dx/dy = (n-1) / sigma^2

# and from this, we can derive the distribution of the standard deviation
hist(stats[2,], breaks=80, xlab="Standard Deviation",
     main="Sampling Distribution of the Standard Deviation", freq=FALSE)
curve(dchisq(y^2 * (n-1) / 10^2, df=n-1) * 2 * y * (n-1) / 10^2, add=TRUE, lwd=5, xname="y")

# x = sd^2 * (n-1) / sigma^2 ~ chi^2(df=n-1)
# y = sqrt(x / (n-1) * sigma^2) (here y is then the standard deviation)
# x = y^2 * (n-1) / sigma^2
# dx/dy = 2*y * (n-1) / sigma^2

# now let's look at the bivariate sampling distribution of the mean and
# standard deviation; we will use 2-dimensional kernel density estimation for
# this and create 3-dimension perspective plot based on that
library(MASS)
res <- kde2d(stats[1,], stats[2,], n=50)
persp(res, xlab="Mean", ylab="Standard Deviation", zlab="Density",
      col="gray80", border="gray50", ticktype="detailed",
      theta=135, phi=35, shade=0.7, ltheta=135)

# when the raw data are normally distributed, then the mean and standard
# deviation are independent of each other
cor(stats[1,], stats[2,])