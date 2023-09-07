############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-09-07
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 4.2 - ?
#
# last updated: 2023-09-07

############################################################################

### 4.2: Estimates, standard errors, and confidence intervals

# remember last time, we considered the (toy) example of a population
# consisting of N=10 individuals with heights equal to:
y <- c(178, 184, 165, 173, 196, 168, 171, 185, 180, 174)

# using combn(), we can generate all possible samples of size n=3
sampdist <- apply(combn(10, 3), 2, function(i) y[i])
sampdist

# say we are interested in the mean of the population (the parameter/estimand)
mean(y)

# take the mean of each column in the sampling distribution of the data
means <- apply(sampdist, 2, mean)
means

# note: we could also do the above with
colMeans(sampdist)

# the means vector is the sampling distribution of the mean (the statistic) in
# this particular example; let's create a histogram of this distribution
hist(means, main="Sampling Distribution of the Mean", xlab="Mean")

## Standard errors, inferential uncertainty, and confidence intervals

# interestingly, the mean of the means in the sampling distribution is equal
# to the parameter we are estimating
mean(means)

# the standard deviation of the statistic in our sampling distribution is
# called the 'standard error' of the statistic
sd(means)

# we could also be interested in the range (i.e., the difference between the
# maximum and minimum value in our sample) as a statistic
ranges <- apply(sampdist, 2, function(x) max(x) - min(x))
ranges

# create a histogram of the sampling distribution of the range
hist(ranges, main="Sampling Distribution of the Range", xlab="Range", breaks=10)

# the standard error of the range
sd(ranges)

# note: for the range, the mean of the sampling distribution is way lower than
# the corresponding parameter in the population
max(y) - min(y)
mean(ranges)

# sampling distribution of the variance
variances <- apply(sampdist, 2, function(x) var(x))
variances
hist(variances, main="Sampling Distribution of the Variance", xlab="Variance")
sd(variances)
mean(variances)
var(y)

# note: for the mean and variance, the mean of the values in the respective
# sampling distribution is equal to the corresponding parameter; so for these
# two statistics, the statistic is an 'unbiased estimator' of the
# corresponding parameter (but not for the range, as we saw above)

# illustrate how the variation of the statistic (i.e., the standard error)
# gets smaller as we increase the sample size
par(mfrow=c(2,2))
ns <- c(3, 5, 7, 10)
for (n in ns) {
   sampdist <- apply(combn(10, n), 2, function(i) y[i])
   means <- apply(sampdist, 2, mean)
   hist(means, main=paste0("Sampling Distribution of the Mean (n=", n, ")"),
        xlab="Mean", breaks=seq(168, 190, by=2))
}

# when the standard error of the statistic goes to zero as we increase the
# sample size, then we say that the estimator is consistent

# blah blah blah