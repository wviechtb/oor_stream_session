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
n <- 12
sigma <- 10
stats <- replicate(100000, {
   x <- rnorm(n, mean=175, sd=sigma)
   c(mean(x), sd(x))
})

# the first row of stats includes the means, so look at the corresponding
# sampling distribution and superimpose the theoretical one
hist(stats[1,], breaks=80, xlab="Mean",
     main="Sampling Distribution of the Mean", freq=FALSE)
curve(dnorm(x, mean=175, sd=sigma/sqrt(n)), add=TRUE, lwd=5)

# compute the standard deviation of the means (i.e., the standard error of the mean)
sd(stats[1,])

# based on statistical theory, we can derive the equation for the standard
# error of the mean
sigma / sqrt(n)

# the second row includes the standard deviations, so look at the
# corresponding sampling distribution
hist(stats[2,], breaks=80, xlab="Standard Deviation",
     main="Sampling Distribution of the Standard Deviation", freq=FALSE)

# check that the distribution of the scaled variances is really a chi-squared
# distribution with n-1 degrees of freedom
hist(stats[2,]^2 * (n-1) / sigma^2, breaks=80, xlab="Variance * (n-1) / sigma",
     main="Sampling Distribution of the Scaled Variance", freq=FALSE)
curve(dchisq(x, df=n-1), add=TRUE, lwd=5)

# from this, we can derive the distribution of the variance
hist(stats[2,]^2, breaks=80, xlab="Variance",
     main="Sampling Distribution of the Variance", freq=FALSE)
curve(dchisq(y * (n-1) / sigma^2, df=n-1) * (n-1) / sigma^2, add=TRUE, lwd=5, xname="y")

# https://online.stat.psu.edu/stat414/lesson/23/23.1
# x = sd^2 * (n-1) / sigma^2 ~ chi^2(df=n-1)
# y = x / (n-1) * sigma^2 (here y is then the variance)
# x = y * (n-1) / sigma^2
# dx/dy = (n-1) / sigma^2

# and from this, we can derive the distribution of the standard deviation
hist(stats[2,], breaks=80, xlab="Standard Deviation",
     main="Sampling Distribution of the Standard Deviation", freq=FALSE)
curve(dchisq(y^2 * (n-1) / sigma^2, df=n-1) * 2 * y * (n-1) / sigma^2, add=TRUE, lwd=5, xname="y")

# x = sd^2 * (n-1) / sigma^2 ~ chi^2(df=n-1)
# y = sqrt(x / (n-1) * sigma^2) (here y is then the standard deviation)
# x = y^2 * (n-1) / sigma^2
# dx/dy = 2*y * (n-1) / sigma^2

# compute the standard deviation of the standard deviations (i.e., the
# standard error of the standard deviation)
sd(stats[2,])

# based on a bunch of tedious derivations (not shown), we can derive an
# approximate equation for the true standard error of the standard deviation
sigma / sqrt(2*(n-1))

# they are not exactly the same because we only simulated 100,000 values plus
# the equation that was derived is just an approximation

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

# construct the variance-covariance matrix of the two statistics
var(t(stats))

## Confidence intervals from the t distribution

# first draw a standard normal distribution
xs <- seq(-4, 4, length=10000)
ys <- dnorm(xs, mean=0, sd=1)
plot(xs, ys, type="l", bty="l", lty="dotted")

# calculate the density of a t-distribution with df=5 and add this to the plot
xs <- seq(-4, 4, length=10000)
ys <- dt(xs, df=5)
lines(xs, ys, type="l", lwd=2)

# add a legend
legend("topright", inset=.01, lty=c("dotted","solid"), lwd=c(1,2),
       legend=c("standard normal", "t-distribution (df=5)"))

# a simple animation showing how a t-distributions looks more and more like a
# standard normal when the degrees of freedom get large
for (i in 1:50) {
   xs <- seq(-4, 4, length=10000)
   ys <- dnorm(xs, mean=0, sd=1)
   plot(xs, ys, type="l", bty="l", lty="dotted")
   xs <- seq(-4, 4, length=10000)
   ys <- dt(xs, df=i)
   lines(xs, ys, type="l", lwd=2)
   legend("topright", inset=.01, lty=c("dotted","solid"), lwd=c(1,2),
          legend=c("standard normal", paste0("t-distribution (df=",i,")")))
   Sys.sleep(0.2)
}

# the example data (the weight of some object is measured 5 times)
y <- c(35, 34, 38, 35, 37)

# mean and standard deviation of the 5 measurements
mean(y)
sd(y)

# construct the 95% confidence interval for the true mean
n <- length(y)
se <- sd(y) / sqrt(n)
mean(y) + qt(c(0.025, 0.975), n-1) * se

## Inference for discrete data

# create the data for the example
y <- sample(rep(c(0,1,2,3,4), c(600,300,50,30,20)))
y

# note: calling this 'nonbinary discrete data' is a bit odd; this is simply a
# count variable; if we are interested in the mean count, then we can proceed
# as above

# mean and standard deviation of these values
mean(y)
sd(y)

# construct the 95% confidence interval for the true mean
n <- length(y)
se <- sd(y) / sqrt(n)
ci <- round(mean(y) + qt(c(0.025, 0.975), n-1) * se, digits=2)
ci

## Linear transformations

# estimated number of pet dogs in a city of one million
mean(y) * 1000000

# confidence interval for the number of pet dogs in a city of one million
ci * 1000000

# another example; say we measure the height of 10 individuals (in cm)
cm <- c(178, 184, 165, 173, 196, 168, 171, 185, 180, 174)

# compute the mean height and the corresponding confidence interval
mean(cm)
n <- length(cm)
se <- sd(cm) / sqrt(n)
ci.cm <- mean(cm) + qt(c(0.025, 0.975), n-1) * se
round(ci.cm, digits=2)

# transform the values into heights in inches
inches <- cm * 0.393701

# compute the mean height and the corresponding confidence interval
mean(inches)
se <- sd(inches) / sqrt(n)
ci.inches <- mean(inches) + qt(c(0.025, 0.975), n-1) * se
round(ci.inches, digits=2)

# show that the confidence interval for inches can be obtained by transforming
# the bounds of the confidence interval for centimeters
round(ci.cm * 0.393701, digits=2)

## Comparisons, visual and numerical

# read in the dataset, turn it into a proper data frame, and give the
# variables proper names (support = percent that supports the death penalty;
# nosupport = percent that does not; noopinion = percent that has no opinion
# on the matter)
dat <- data.frame(matrix(scan("polls.dat"), ncol=5, byrow=TRUE))
names(dat) <- c("year", "month", "support", "nosupport", "noopinion")
dat

# compute the proportion of people that support the death penalty among the
# people who do have an opinion on the matter
dat$support2 <- with(dat, support / (support + nosupport))
dat

# compute the year + fraction of the year based on the month variable
dat$date <- dat$year + dat$month / 12
dat

# recreate Figure 4.3
plot(dat$date, dat$support2*100, pch=19, xlab="Year",
     ylab="Percentage support for the death penalty")

# compute the standard error for each proportion
dat$se <- sqrt(dat$support2*(1-dat$support2)/1000)
dat

# add the 68% confidence interval bounds around each point
apply(dat, 1, function(x) segments(x["date"], 100*(x["support2"] - x["se"]),
                                   x["date"], 100*(x["support2"] + x["se"])))
