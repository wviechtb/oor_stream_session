############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-09-19
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 3.3 - ?
#
# last updated: 2024-09-19

############################################################################

### 3.3: Sampling to simulate prediction

# probabilities of seeing 0, 1, or 2 times water when the true probability is
# 0.7 based on a binomial distribution
cbind(W=0:2, prob=dbinom(0:2, size=2, prob=0.7))

# simulate one value of W from this distribution
rbinom(1, size=2, prob=0.7)

# simulate 10 values of W from this distribution
rbinom(10, size=2, prob=0.7)

# simulate 100,000 values and create a frequency table of the observed values
dummy_w <- rbinom(1e5, size=2, prob=0.7)
table(dummy_w)

# turn the frequencies into proportions
table(dummy_w) / 1e5

# simulate 100,000 values when there are 9 tosses
dummy_w <- rbinom(1e5, size=9, prob=0.7)
table(dummy_w)

# plot the frequencies
plot(table(factor(dummy_w, levels=0:9)))
