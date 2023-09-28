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

# let's repeat again what we essentially did last time and simulate 100
# observations from a normal distribution with true mean 175 and true standard
# deviation 10 and then compute the mean and standard deviation and then
# repeat this 100,000 times
stats <- replicate(100000, {
   x <- rnorm(100, mean=175, sd=10)
   c(mean(x), sd(x))
})

# the first row of stats are the means, the second row are the SDs
hist(stats[1,], breaks=100, xlab="Mean",
     main="Sampling Distribution of the Mean", freq=FALSE)
