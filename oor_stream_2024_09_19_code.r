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

