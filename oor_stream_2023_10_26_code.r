############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-10-26
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 4.3 - ?
#
# last updated: 2023-10-26

############################################################################

### 4.3: Bias and unmodeled uncertainty

# Bias in estimation

# in the population, the mean number of hours watched by men and women
# combined (where there is an equal number of men and women) is 2.75
pop.mean <- 0.5 * 3.0 + 0.5 * 2.5

# according to recent data, men in the US watch around 3 hours, women around
# 2.5 hours per day of television (and let's assume a standard deviation of 0.5)
means <- replicate(100000, {
   hrs.m <- rnorm( 50, mean=3.0, sd=0.5)
   hrs.w <- rnorm(150, mean=2.5, sd=0.5)
   mean(c(hrs.m, hrs.w))
})

hist(means, main="Sampling Distribution of the Mean")
