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

## 3.3.1: Dummy data

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

# Figure 3.5: plot of the frequencies
plot(table(factor(dummy_w, levels=0:9)))

# load the rethinking package
library(rethinking)

# could also use the simplehist() function from the rethinking package
simplehist(dummy_w, xlab="dummy water count")

## 3.3.2: Model checking

# 3.3.2.1: Did the software work?

# not done since the model is too simple

# 3.3.2.2: Is the model adequate?

# recreate the grid approximation we did in chapter 2
p_grid <- seq(from=0, to=1, length.out=1000) # set up the grid
prob_p <- rep(1, 1000) # assumed prior (each value of p is equally likely)
prob_data <- dbinom(6, size=9, prob=p_grid) # compute the likelihoods
posterior <- prob_data * prob_p # compute the posterior values
posterior <- posterior / sum(posterior) # rescale them so they add up to 1
plot(p_grid, posterior, type="l", lwd=4) # plot the posterior distribution

# note that the peak of the posterior is at 6/9
abline(v=6/9)

# for every value of p in the grid, construct the binomial distribution
mat <- sapply(p_grid, function(p) dbinom(0:9, size=9, prob=p))
mat[,1:5]

# multiple the probabilities of seeing 0:9 times water for a given value of p
# with the corresponding posterior probability of the value of p
mat <- t(posterior * t(mat))
mat[,1:5]

# now take the mean across rows; this gives us the posterior predictive distribution
ppd <- rowMeans(mat)
ppd <- ppd / sum(ppd)

# Figure 3.6: plot of the posterior predictive distribution
plot(0:9, ppd, type="h", lwd=3, xlab="", ylab="probability", xaxt="n", ylim=c(0,0.25))
axis(side=1, 0:9)

# if we ignore the uncertainty as to what p is and just use the most probable
# value according to the posterior distribution for p, then we would be
# underestimating the uncertainty for new observations
lines(0:9 + 0.05, dbinom(0:9, size=9, prob=6/9), type="h", lwd=3, col="red")
