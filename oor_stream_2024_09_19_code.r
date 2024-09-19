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
plot(0:9, ppd, type="h", lwd=5, xlab="", ylab="probability", xaxt="n", ylim=c(0,0.3))
axis(side=1, 0:9)

# if we ignore the uncertainty as to what p is and just use the most probable
# value according to the posterior distribution for p, then we would be
# underestimating the uncertainty for new observations
lines(0:9 + 0.05, dbinom(0:9, size=9, prob=6/9), type="h", lwd=3, col="red")

# now suppose we just have 10,000 sampled values from the posterior distribution
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

# now we are going to simulate 10,000 new data points where for each simulated
# value, we use the corresponding sampled value of p from the posterior
w <- rbinom(1e4, size=9, prob=samples)
head(w)

# turn w into a factor with levels 0:9; that way, if a particular value never
# occurs in the simulated data, then the frequency table below will still show
# this value with 0 frequency; this is probably not needed here, but we still
# do this to be on the safe side
w <- factor(w, levels=0:9)

# create a frequency table of the simulated values
tab <- table(w)
tab

# rescale the frequencies to proportions
tab <- tab / sum(tab)
tab

# add these proportions to the figure
lines(0:9 - 0.05, tab, type="h", lwd=3, col="blue")

# add a legend
legend("topleft", inset=.01, lty=1, col=c("black","red","blue"), lwd=c(5,3,3),
       legend=c("posterior predictive distribution",
                "binomial(size=9, p=6/9)",
                "posterior predictive distribution simulations"))

# now we can use the simulated values from the posterior predictive
# distribution to compute for example a 80% percentile interval
quantile(w, probs=c(.10,.90))

# simulate values from the PPD of the maximum run lengths
sim.maxrun <- sapply(samples, function(p) {
   x <- rbinom(9, size=1, prob=p)
   maxrun <- max(rle(x)$lengths)
   factor(maxrun, levels=1:9)
})

# create a frequency table of the simulated values and rescale
tab <- table(sim.maxrun)
tab <- tab / sum(tab)
tab

# Figure 3.7 (left): plot the posterior predictive distribution
plot(1:9, c(tab), type="h", lwd=5, xlab="", ylab="probability", xaxt="n")
axis(side=1, 1:9)

# actual sequence observed
wobs <- c(1,0,1,1,1,0,1,0,1)

# maximum running length observed in the actual sequence
maxrun <- max(rle(wobs)$lengths)

# make the line in the plot blue
segments(maxrun, 0, maxrun, tab[which(maxrun == names(tab))], lwd=8, col="#1e59ae")

# simulate values from the PPD of the number of switches
sim.switches <- sapply(samples, function(p) {
   x <- rbinom(9, size=1, prob=p)
   switches <- length(rle(x)$lengths) - 1
   factor(switches, levels=0:8)
})

# create a frequency table of the simulated values and rescale
tab <- table(sim.switches)
tab <- tab / sum(tab)
tab

# Figure 3.7 (right): plot the posterior predictive distribution
plot(0:8, c(tab), type="h", lwd=5, xlab="", ylab="probability", xaxt="n")
axis(side=1, 0:8)

# number of switches observed in the actual sequence
switches <- length(rle(wobs)$lengths) - 1

# make the line in the plot blue
segments(switches, 0, switches, tab[which(switches == names(tab))], lwd=8, col="#1e59ae")

############################################################################


plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxs="i", yaxs="i")
rect(0, 0, 1, 1, col="#1e59ae")
rect(0.5, 0.5, 1, 1, col="brown4")

steps <- 1000

pos <- runif(2)

for (i in 1:steps) {

   pos.old <- pos
   pos <- pos + runif(2, -.05, .05)
   if (pos[1] > 1)
      pos[1] <- pos[1] - 1
   if (pos[1] < 0)
      pos[1] <- 1 - pos[1]
   if (pos[2] > 1)
      pos[2] <- pos[2] - 1
   if (pos[2] < 0)
      pos[2] <- 1 - pos[2]
   segments(pos.old[1], pos.old[2], pos[1], pos[2])

}




############################################################################
