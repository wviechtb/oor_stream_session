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

# compute the chances of observing a proportion of 0.7 or higher
sum(tab[props >= 0.7])

# compute the chances of observing a proportion of 0.75 or higher
sum(tab[props >= 0.75])


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
