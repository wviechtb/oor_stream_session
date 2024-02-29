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
n_sims <- 1000
n_girls <- rep(NA, n_sims)
for (s in 1:n_sims) {
   n_girls[s] <- rbinom(1, 400, 0.488)
}

# create a histogram of the simulated values
hist(n_girls, main="", xlab="Number of Girls (out of 400)")

# we don't really need a for-loop to do the above; we can directly simulate
# 1000 values from the binomial distribution; to make the simulated values
# reproducible, we also set the seed of the random number generator
set.seed(1234)
n_girls <- rbinom(n_sims, 400, 0.488)
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
   } else if (birth_type[i] == "identical twin") {
      girls[i] <- 2*rbinom(1, 1, 0.495)
   } else {
      girls[i] <- rbinom(1, 2, 0.495)
   }
}

n_girls <- sum(girls)
n_girls

# can also use a doubly-nested ifelse() construction to do the above

girls <- ifelse(birth_type=="single birth", rbinom(400, 1, 0.488),
                ifelse(birth_type=="identical twins", 2*rbinom(400, 1, 0.495),
                       rbinom(400, 2, 0.495)))
n_girls <- sum(girls)
n_girls

# simulate the process above 1000 times

n_girls <- rep(NA, n_sims)

for (s in 1:1000) {

   birth_type <- sample(c("fraternal twin","identical twin","single birth"),
                        size=400, replace=TRUE, prob=c(1/125, 1/300, 1-1/125-1/300))

   girls <- rep(NA, 400)

   for (i in 1:400) {
      if (birth_type[i] == "single birth") {
         girls[i] <- rbinom(1, 1, 0.488)
      } else if (birth_type[i] == "identical twin") {
         girls[i] <- 2*rbinom(1, 1, 0.495)
      } else {
         girls[i] <- rbinom(1, 2, 0.495)
      }
   }

   n_girls[s] <- sum(girls)

}

# create a histogram of the simulated values
hist(n_girls, main="", xlab="Number of Girls (out of 400)")

############################################################################

### 5.2: Simulation of continuous and mixed discrete/continuous models

# https://en.wikipedia.org/wiki/Normal_distribution
# https://en.wikipedia.org/wiki/Log_normal_distribution
# https://en.wikipedia.org/wiki/Binomial_distribution
# https://en.wikipedia.org/wiki/Poisson_distribution

n_sims <- 1000
y1 <- rnorm(n_sims, mean=3, sd=0.5)
y2 <- rlnorm(n_sims, meanlog=3, sdlog=0.5)
y3 <- rbinom(n_sims, size=20, prob=0.6)
y4 <- rpois(n_sims, lambda=5)

# Figure 5.2

par(mfrow=c(2,2))
hist(y1, breaks=seq(floor(min(y1)), ceiling(max(y1)), 0.2), main="normal dist with mean 3 and sd 0.5")
hist(y2, breaks=seq(0, ceiling(max(y2)) + 5, 5), main="lognormal dist with logmean 3 and logsd 0.5")
hist(y3, breaks=seq(-0.5, 20.5, 1), main="binomial dist with 20 tries and probability 0.6")
hist(y4, breaks=seq(-0.5, max(y4) + 1, 1), main="Poisson dist with mean 5")
par(mfrow=c(1,1))

# generate the height of one randomly chosen adult

male <- rbinom(1, 1, 0.48)
height <- ifelse(male==1, rnorm(1, 69.1, 2.9), rnorm(1, 64.5, 2.7))

# generate the heights of 10 adults and take the mean of the 10 simulated values

N <- 10
male <- rbinom(N, 1, 0.48)
height <- ifelse(male==1, rnorm(N, 69.1, 2.9), rnorm(N, 64.5, 2.7))
avg_height <- mean(height)
avg_height

# repeat the above 1000 times to generate 1000 means (and also save the
# maximum height of the 10 adults in each iteration)

n_sims <- 1000
avg_height <- rep(NA, n_sims)
max_height <- rep(NA, n_sims)
N <- 10

for (s in 1:n_sims) {
   male <- rbinom(N, 1, 0.48)
   height <- ifelse(male==1, rnorm(N, 69.1, 2.9), rnorm(N, 64.5, 2.7))
   avg_height[s] <- mean(height)
   max_height[s] <- max(height)
}

# create a histogram of the simulated means
hist(avg_height, main="Dist of avg height of 10 adults", xlab="Average Height")

# create a histogram of the simulated maximums
hist(max_height, main="Dist of the max height of 10 adults", xlab="Max Height")


############################################################################
