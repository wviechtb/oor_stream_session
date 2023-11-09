############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-11-09
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 4.4 - ?
#
# last updated: 2023-11-09

############################################################################

### 4.4: Statistical significance, hypothesis testing, and statistical errors

## Statistical significance

# say we flip a coin 20 times and it is a fair coin (so 50% chance for heads
# and 50% chance for tails); then we can construct the sampling distribution
# of the proportion of heads observed in the 20 flips through simulation
set.seed(1234)
props <- rbinom(10000000, size=20, prob=0.5) / 20

# frequency table of the observed proportions
tab <- table(props)
tab

# examine the sampling distribution of the proportions
plot(tab, type="h", lwd=3, bty="l", xlab="Proportion", ylab="Frequency",
     main="Sampling Distribution of the Proportion")

# convert the frequency table of the observed proportions into the
# corresponding probabilities of observing the proportions
tab <- tab / 10000000
tab

# extract from 'tab' the actually observed proportions (as a numeric vector)
props <- as.numeric(names(tab))
props

# compute the probability of observing a proportion of 0.7
sum(tab[props == 0.7])

# compute the probability of observing a proportion of 0.7 or higher
sum(tab[props >= 0.7])

# compute the probability of observing a proportion of 0.75 or higher
sum(tab[props >= 0.75])

# compute the probability of observing a proportion of 0.25
sum(tab[props == 0.25])

# compute the probability of observing a proportion of 0.25 or lower
sum(tab[props <= 0.25])

# now imagine you do the experiment (flipping the coin 20 times and observing
# the proportion of heads) once and you get the following result
heads <- c(T, F, F, F, F, T, F, F, T, T, F, F, T, F, T, T, F, F, T, F)
mean(heads)

# is this outcome unusual if the coin is fair?

# compute the probability of observing this result under the sampling
# distribution of a proportion for a fair coin
sum(tab[props == mean(heads)])

# compute the probability of observing this result or an even more extreme
# deviation from 0.5 under the sampling distribution of a proportion for a
# fair coin
sum(tab[props <= mean(heads)])

# this is not an unusual event to happen if the coin is really fair

# the probability computed above is the (one-sided) p-value of our observed
# result for testing the null hypothesis H0: the coin is fair

# but say we had observed the following result
heads <- c(F, F, F, F, F, T, F, F, F, T, F, F, F, F, F, T, F, F, F, F)
mean(heads)

# the probability of observing this result or an even more extreme one is very
# small (i.e., the p-value is very small)
sum(tab[props <= mean(heads)])

# this may make use question whether the coin is really fair; conventionally,
# we are going to reject the null hypothesis if the p-value is .05 or smaller

# we know that the standard error of a proportion based on a sample size of 20
# (if the true proportion is 0.5) is given by the following equation (see
# section 4.2: Standard errors and confidence intervals for averages and
# proportions)
se <- sqrt(0.5 * 0.5 / 20)
se

# compute the test statistic (how far away is the observed result from the
# value under the null hypothesis, relative to the standard error of the
# statistic); if this is large (say, +-2), then again we are going to reject
# the null hypothesis
(mean(heads) - 0.5) / se

############################################################################





# we don't actually need to simulate these proportions to construct the
# sampling distribution in this example, since we know based on statistical
# theory that the probabilities of observing these different proportions can
# be computed based on a binomial distribution
pr <- dbinom(0:20, size=20, prob=0.5)
pr

# examine the sampling distribution of the proportions
plot((0:20)/20, pr, type="h", lwd=3, bty="l", xlab="Proportion", ylab="Frequency",
     main="Sampling Distribution of the Proportion")
