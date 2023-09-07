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

# the standard deviation of the statistic in our sampling distribution is
# called the 'standard error' of the statistic
sd(means)

# we could also be interested in the range (i.e., the difference between the
# maximum and minimum value in our sample) as a statistic
ranges <- apply(sampdist, 2, function(x) max(x) - min(x))
ranges

# create a histogram of the sampling distribution of the range
hist(ranges, main="Sampling Distribution of the Range", xlab="Range")

# the standard error of the range
sd(ranges)
