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

## How many girls in 400 births?

# simulate a single draw from a binomial distribution with 400 'trials'
# (births) and a probability of .488 that the event of interest occurs on a
# single trial (i.e., that the baby is a girl)
n_girls <- rbinom(1, 400, 0.488)
n_girls

# repeat this process 1000 times and score the simulated values in a vector
sims <- 1000
n_girls <- rep(NA, sims)
for (s in 1:sims) {
   n_girls[s] <- rbinom(1, 400, 0.488)
}

# create a histogram of the simulated values
hist(n_girls, main="", xlab="Number of Girls (out of 400)")

# we don't really need a for-loop to do the above; we can directly simulate
# 1000 values from the binomial distribution; to make the simulated values
# reproducible, we also set the seed of the random number generator
set.seed(1234)
n_girls <- rbinom(sims, 400, 0.488)
n_girls

# create a histogram of the simulated values
hist(n_girls, main="", xlab="Number of Girls (out of 400)")

## Accounting for twins

# simulate the process of 400 births where there is a certain chance of twins
# being born and save how many of the babies born on each birth are girls
birth_type <- sample(c("fraternal twin","identical twin","single birth"),
                     size=400, replace=TRUE, prob=c(1/125, 1/300, 1-1/125-1/300))
girls <- rep(NA, 400)
for (i in 1:400) {
   if (birth_type[i] == "single birth") {
      girls[i] <- rbinom(1, 1, 0.488)
   } else if (birth_type[j] == "identical twin") {
      girls[i] <- 2*rbinom(1, 1, 0.495)
   } else {
      girls[i] <- rbinom(1, 2, 0.495)
   }
}
n_girls <- sum(girls)
n_girls

# simulate the process above 1000 times

n_girls <- rep(NA, sims)

for (s in 1:1000) {

   birth_type <- sample(c("fraternal twin","identical twin","single birth"),
                        size=400, replace=TRUE, prob=c(1/125, 1/300, 1-1/125-1/300))
   girls <- rep(NA, 400)
   for (i in 1:400) {
      if (birth_type[i] == "single birth") {
         girls[i] <- rbinom(1, 1, 0.488)
      } else if (birth_type[j] == "identical twin") {
         girls[i] <- 2*rbinom(1, 1, 0.495)
      } else {
         girls[i] <- rbinom(1, 2, 0.495)
      }
   }
   n_girls <- sum(girls)


}



girls <- ifelse(birth_type=="single birth", rbinom(400, 1, 0.488),
  ifelse(birth_type=="identical twins", 2*rbinom(400, 1, 0.495),
  rbinom(400, 2, 0.495)))


############################################################################
