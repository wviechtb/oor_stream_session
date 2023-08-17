############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-08-17
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 3.6 - ?
#
# last updated: 2023-08-17

############################################################################

### 3.6: Probability modeling

## Using an empirical forecast

# simulate y (proportion of the votes received by one of the candidates) 10^7
# times and then compute the probability of seeing exactly 1000 votes for each
# candidate in 2000 voters (this gives an empirical estimate of seeing an
# equal split of the votes using a simulation approach)
n <- 2000
y <- rnorm(10000000, mean=0.49, sd=0.04)
mean(round(y * n) == 1000)

dnorm(0.5, mean=0.49, sd=0.04) / n
