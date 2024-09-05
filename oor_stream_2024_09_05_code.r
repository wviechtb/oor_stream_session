############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-09-05
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 3.1 - 3.2
#
# last updated: 2024-09-05

############################################################################

# code for the vampire test example

Pr_Positive_Vampire <- 0.95 # also called the sensitivity of the test
Pr_Positive_Mortal  <- 0.01 # 1-this is called the specificity of the test (0.99)
Pr_Vampire <- 0.001
Pr_Positive <- Pr_Positive_Vampire * Pr_Vampire +
               Pr_Positive_Mortal  * (1 - Pr_Vampire)
Pr_Vampire_Positive <- Pr_Positive_Vampire * Pr_Vampire / Pr_Positive
Pr_Vampire_Positive

#                  vampire      mortal
#               +-----------+-----------+
# test positive | (3)    95 | (5)   999 |   1094 (7)
#               +-----------+-----------+
# test negative | (4)     5 | (6) 98901 |  98906
#               +-----------+-----------+-----------
#                 (2)   100       99900 | 100000 (1)
#
# (1) Let's assume we are looking at a population of 100,000 individuals.
# (2) Given Pr_Vampire, we can then compute how many of them are vampires and
#     how many are mortals.
# (3) Given Pr_Positive_Vampire, we can then compute how many of the vampires
#     will test positive.
# (4) We can then also fill in this number.
# (5) Given Pr_Positive_Mortal, we can then compute how many of the mortals
#     will test positive.
# (6) We can then also fill in this number.
# (7) We can now also fill in the row totals.
# (8) Now that we have completed the table, we can easily compute the
#     probability that a person is a vampire given that the test is positive:
#     namely, 95 / 1094

95 / 1094

# one could then take the additional step of setting N in (1) to 1, since then
# all of the numbers in the calculations above can be thought of as
# probabilities and we can then see how the equation above arises

############################################################################

### 3.1: Sampling from a grid-approximate posterior

# load the rethinking package
library(rethinking)

# recreate the grid approximation we did in chapter 2
p_grid <- seq(from=0, to=1, length.out=1000)
prob_p <- rep(1, 1000) # prior
prob_data <- dbinom(6, size=9, prob=p_grid) # likelihood
plot(p_grid, prob_data, type="l") # plot the likelihood function
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)
plot(p_grid, posterior, type="l") # plot the posterior distribution

# sample values of p in accordance with how probable the values are (which we
# have determined above using our grid approximation)
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

# Figure 3.1
par(mfrow=c(1,2))
plot(samples, pch=19, ylim=c(0,1), xlab="sample number",
     ylab="proportion water (p)", col=rgb(0,0,0.5,0.1))
dens(samples, xlim=c(0,1), lwd=3, col="blue", xlab="proportion water (p)")

# superimpose the posterior grid approximation (note: we have to rescale the
# posterior values in such a way that the grid approximation can be treated
# like a proper density, which has an area of 1 under the curve; we can
# accomplish this by multiplying the posterior values by (n-1) where n is the
# number of values we used in our grid approximation)
lines(p_grid, 999*posterior)
par(mfrow=c(1,1))

# another way to visualize the samples (like in the left part of Figure 3.1,
# but not drawing points but instead connecting them via lines)
plot(samples, pch=19, cex=0, type="o")

############################################################################

### 3.2: Sampling to summarize

## 3.2.1: Intervals of defined boundaries

# add up posterior probability where p < 0.5
sum(posterior[p_grid < 0.5])

# compute the proportion of sampled values that are below 0.5
sum(samples < 0.5) / 1e4

# compute the proportion of samples values that are above 0.5 and below 0.75
sum(samples > 0.5 & samples < 0.75) / 1e4

## 3.2.2: Intervals of defined mass

# determine under which value of p are 80% of the sampled values
p.8 <- quantile(samples, 0.8)
p.8

# check that indeed 80% of the sampled values are below this cutoff (note:
# this is not exactly 80% because the grid values from which we sampled are
# discrete and can repeated themselves)
sum(samples < p.8) / 1e4

# note: we could also do this kind of calculation based on the grid
# approximation by finding the value of p at which point the cumulative sum of
# the posterior probability values add up to 0.8
p_grid[min(which(cumsum(posterior) > 0.8))]

# determine under which value of p are 10% of the sampled values and under
# which value of p are 90% of the sampled values
p.1 <- quantile(samples, 0.1)
p.9 <- quantile(samples, 0.9)
p.1
p.9

# Figure 3.2
par(mfrow=c(2,2))
plot(p_grid, posterior, type="l", xlab="proportion water (p)", ylab="Density")
sel <- p_grid < 0.5
polygon(c(p_grid[sel], rev(p_grid[sel])), c(posterior[sel], rep(0,sum(sel))), col="dodgerblue")
plot(p_grid, posterior, type="l", xlab="proportion water (p)", ylab="Density")
sel <- p_grid > 0.5 & p_grid < 0.75
polygon(c(p_grid[sel], rev(p_grid[sel])), c(posterior[sel], rep(0,sum(sel))), col="dodgerblue")
plot(p_grid, posterior, type="l", xlab="proportion water (p)", ylab="Density")
sel <- p_grid < p.8
polygon(c(p_grid[sel], rev(p_grid[sel])), c(posterior[sel], rep(0,sum(sel))), col="dodgerblue")
plot(p_grid, posterior, type="l", xlab="proportion water (p)", ylab="Density")
sel <- p_grid > p.1 & p_grid < p.9
polygon(c(p_grid[sel], rev(p_grid[sel])), c(posterior[sel], rep(0,sum(sel))), col="dodgerblue")
par(mfrow=c(1,1))

# grid approximation when we see 3 times land in three tosses
p_grid <- seq(from=0, to=1, length.out=1000)
prob_p <- rep(1, 1000) # prior
prob_data <- dbinom(3, size=3, prob=p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

# sample values from the posterior distribution as we did above
samples <- sample(p_grid, size=1e4, replace=TRUE, prob=posterior)

# plot the posterior
par(mfrow=c(1,2))
plot(p_grid, posterior, type="l", xlab="proportion water (p)", ylab="Density", lwd=2)

# determine the 25th and 75th percentile
p.25 <- quantile(samples, 0.25)
p.75 <- quantile(samples, 0.75)
p.25
p.75

# use the PI() function to obtain the same values
PI(samples, 0.5)

# shade this region in the plot above
sel <- p_grid > p.25 & p_grid < p.75
polygon(c(p_grid[sel], rev(p_grid[sel])), c(posterior[sel], rep(0,sum(sel))), col="dodgerblue")

# we can construct other intervals that also contain 50% of the sampled
# values; for example, this one
quantile(samples, 0.00)
quantile(samples, 0.50)

# note that these intervals are of different widths
unname(quantile(samples, 0.75) - quantile(samples, 0.25))
unname(quantile(samples, 0.50) - quantile(samples, 0.00))

# the first interval is much narrower than the second

# the 50% highest posterior density interval (HPDI) is that interval contains
# 50% of the sampled values and that is the narrowest
HPDI(samples, prob=0.5)

# do a brute-force search across all intervals of the type (p.lo, p.hi) which
# contain 50% of the sampled values starting at (0, .50) to (0.50, 1) and find
# the one that is narrowest
p.lo <- seq(0, 0.5, by=0.01)
p.hi <- p.lo + 0.5
width <- rep(NA, length(p.lo))
for (i in 1:length(p.lo)) {
   width[i] <- quantile(samples, p.hi[i]) - quantile(samples, p.lo[i])
}
quantile(samples, p.lo[which.min(width)])
quantile(samples, p.hi[which.min(width)])

# we get essentially the same interval as we got with HPDI() (except due to
# slightly different definitions of the percentiles)

# again plot the posterior and shade this region in the plot
plot(p_grid, posterior, type="l", xlab="proportion water (p)", ylab="Density", lwd=2)
sel <- p_grid > HPDI(samples, prob=0.5)[1] & p_grid < HPDI(samples, prob=0.5)[2]
polygon(c(p_grid[sel], rev(p_grid[sel])), c(posterior[sel], rep(0,sum(sel))), col="dodgerblue")
par(mfrow=c(1,1))

## 3.2.3: Point estimates

# get the maximum a posteriori (MAP) estimate from the grid approximation
p_grid[which.max(posterior)]

# based on the sampled values, draw the posterior distribution
dens(samples, xlim=c(0,1), lwd=3, col="blue", xlab="proportion water (p)")

# the peak of that distribution is the MAP based on the sampled values (note:
# instead of adj=0.01, we use adj=0.5, which is the same default value that
# dens() uses)
chainmode(samples, adj=0.5)

# show that this values is indeed the peak in the plot
abline(v=chainmode(samples, adj=0.5))

# obtain the mean and median of the sampled values
mean(samples)
median(samples)

# show these values in the plot
abline(v=mean(samples))
abline(v=median(samples))

# draw the posterior based on the grid approximation and again show these
# three point summaries in the plot
plot(p_grid, posterior, type="l", xlab="proportion water (p)", ylab="Density", lwd=2)
abline(v=chainmode(samples, adj=0.5))
abline(v=mean(samples))
abline(v=median(samples))

# the expected loss based on the posterior distribution when we claim that a
# proportion of 0.5 of the Earth is covered in water
sum(posterior*abs(0.5-p_grid))

# compute the expected loss for every value between 0 and 1 for d
loss <- sapply(p_grid, function(d) sum(posterior*abs(d - p_grid)))

# find the value for d for which the expected loss is minimized
p_grid[which.min(loss)]

# this coincides with the posterior median (minor difference due to sampling
# variation)
median(samples)

# Figure 3.4 (right)
plot(p_grid, loss, type="l", xlab="decision", ylab="loss", lwd=3, col="dodgerblue")
points(p_grid[which.min(loss)], min(loss))

# show that the mean minimizes the expected loss when the loss function is (d-p)^2
loss <- sapply(p_grid, function(d) sum(posterior*(d - p_grid)^2))
p_grid[which.min(loss)]
mean(samples)

############################################################################
