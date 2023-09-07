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

# take the mean of each column in the sampling distribution of the data
means <- apply(sampdist, 2, mean)
means

# note: we could also do the above with
colMeans(sampdist)

# the means vector is the sampling distribution of the mean (the statistic) in
# this particular example
