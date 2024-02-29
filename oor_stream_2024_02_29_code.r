############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-02-29
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 5.1 - ?
#
# last updated: 2024-02-29

############################################################################

### 5.1: Simulation of discrete probability models

# simulate a single draw from a binomial distribution with 400 'trials'
# (births) and a probability of .488 that the event of interest occurs on a
# single trial (i.e., that the baby is a girl)
n_girls <- rbinom(1, 400, 0.488)
n_girls

# repeat this process 1000 times and score the simulated values in a vector
n_sims <- 1000
n_girls <- rep(NA, n_sims)
for (i in 1:n_sims) {
   n_girls[i] <- rbinom(1, 400, 0.488)
}

# create a histogram of the simulated values
hist(n_girls, main="", xlab="Number of Girls (out of 400)")

# we don't really need a for-loop to do the above; we can directly simulate
# 1000 values from the binomial distribution
n_girls <- rbinom(n_sims, 400, 0.488)
n_girls

# create a histogram of the simulated values
hist(n_girls, main="", xlab="Number of Girls (out of 400)")

############################################################################
