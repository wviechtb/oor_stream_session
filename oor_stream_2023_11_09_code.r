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
props <- rbinom(100000, size=20, prob=0.5) / 20

# frequency table of the observed proportions
tab <- table(props)
tab

# examine the sampling distribution of the proportions
plot(tab, type="h", lwd=3, bty="l", xlab="Proportion", ylab="Frequency",
     main="Sampling Distribution of the Proportion")

