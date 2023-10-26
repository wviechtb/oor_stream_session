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

## Bias in estimation

# according to recent data, men in the US watch around 3 hours, women around
# 2.5 hours per day of television; hence, in the population, the mean number
# of hours watched by men and women combined (where there is an equal number
# of men and women) is 2.75
pop.mean <- 0.5 * 3.0 + 0.5 * 2.5
pop.mean

# now let's simulate the situation where we take a sample of n=200 but women
# are more willing to participate and hence are over-represented in the
# sample; let's also assume a standard deviation of 0.5 for the number of
# hours watched within each group; we repeat this process 100000 times, in
# essence generating the sampling distribution of the mean in this manner
set.seed(1234)
means <- replicate(100000, {
   hrs.m <- rnorm( 50, mean=3.0, sd=0.5)
   hrs.w <- rnorm(150, mean=2.5, sd=0.5)
   mean(c(hrs.m, hrs.w))
})

# look at the sampling distribution of the mean
hist(means, main="Sampling Distribution of the Mean", breaks=50)

# add the population mean as a vertical line to the histogram
abline(v=pop.mean, lwd=5)

# show that the sample mean is a biased estimator of the population mean
mean(means)

# in fact, given the above, we know that the mean of the means in the sampling
# distribution is equal to the following weighted mean
(50 * 3.0 + 150 * 2.5) / 200

# if we weight the values of the men and women appropriately (i.e., by the
# inverse of the sampling probabilities times the known proportions in the
# population), then the mean of these weighted values provides an unbiased
# estimate of the population mean
means <- replicate(100000, {
   hrs.m <- rnorm( 50, mean=3.0, sd=0.5)
   hrs.w <- rnorm(150, mean=2.5, sd=0.5)
   #0.5 * mean(hrs.m) + 0.5 * mean(hrs.w)
   mean(c(4/1*0.5*hrs.m, 4/3*0.5*hrs.w))
})

# look at the sampling distribution of the mean
hist(means, main="Sampling Distribution of the Mean", breaks=50)

# add the population mean as a vertical line to the histogram
abline(v=pop.mean, lwd=5)

# show that the sample mean is a biased estimator of the population mean
mean(means)

############################################################################

## Adjusting inferences to account for bias and unmodeled uncertainty

# simulate the observed proportion of support for a candidate under a binomial
# model 10000 times, assuming that the true probability of support is 0.52
props <- replicate(10000, mean(rbinom(60000, 1, 0.52)))

# look at the sampling distribution of the proportion
hist(props, main="Sampling Distribution of the Proportion", breaks=50)

# as we can see, under this model, there is virtually no chance of seeing a
# proportion that is below 0.5

# compute the standard error of these proportions
sd(props)

# now suppose we do 100 polls with n=600, where the true probability of
# support varies across polls; say these probabilities come from a uniform
# distribution with bounds 0.32 and 0.72 (so on average, the probability is
# still 0.52); in the end, we still compute an overall proportion across these
# 100 polls and also the SD of these proportions
props <- replicate(10000, {
   props <- replicate(100, mean(rbinom(600, 1, runif(1,0.17,0.87))))
   mean(props)
})

# examine the sampling distribution
hist(props, main="Sampling Distribution of the Proportion", breaks=50)

# now we see additional variability in the proportions
sd(props)

# note: to make the SD of these proportions to be even larger, one might have
# to simulate the probabilities above under an even more extreme scenario,
# where the true probability is close to 0 or 1 in some polls

############################################################################

### 4.4: Statistical significance, hypothesis testing, and statistical errors

