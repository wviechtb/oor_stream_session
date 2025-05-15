############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-05-15
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 13.4 - ?
#
# last updated: 2025-05-15

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### 13.4: Latent-data formulation

# see Wikipedia for some details about the logistic distribution:
# https://en.wikipedia.org/wiki/Logistic_distribution
#
# in particular, note that the variance of a random variable following a
# logistic distribution is equal to s^2 * pi^2 / 3, where s is the 'scale'
# parameter

# Figure 13.5: plot of the pdf from a logistic distribution
xs <- seq(-6, 6, length.out=1000)
ys <- dlogis(xs, location=0, scale=1)
plot(xs, ys, type="l", lwd=5, bty="l", xlab="", ylab="")

# let's add to this plot the pdf from a normal distribution with the same mean
# and the same standard deviation
ys <- dnorm(xs, mean=0, sd=pi/sqrt(3))
lines(xs, ys, lwd=5, col="red")

# let's redraw the figure and shade the region from -Inf to 1
xs <- seq(-6, 6, length.out=1000)
ys <- dlogis(xs, location=0, scale=1)
plot(xs, ys, type="l", lwd=5, bty="l", xlab="", ylab="")
ys <- ys[xs <= 1]
xs <- xs[xs <= 1]
polygon(c(xs,rev(xs)), c(ys,rep(0,length(xs))), col="gray80", border=NA)
lines(xs, ys, lwd=5)

# the size of the shaded region corresponds to the probability of seeing a
# value between -Inf and 1 under such a logistic distribution; we can compute
# this probability via simulation
z <- rlogis(10^7, location=0, scale=1)
mean(z < 1)

# but we can compute this exactly using plogis()
plogis(1)

# logit^-1(x) = exp(x) / (1 + exp(x))
exp(1) / (1 + exp(1))

# Figure 13.6: pdf for a logistic distribution with mean (location) equal to
# -1.40 + 0.33 * 1 = -1.07 with the region > 0 shaded
xs <- seq(-6, 6, length.out=1000)
ys <- dlogis(xs, location=-1.07, scale=1)
plot(xs, ys, type="l", lwd=5, bty="l", xlab="", ylab="")
ys <- ys[xs > 0]
xs <- xs[xs > 0]
polygon(c(xs,rev(xs)), c(ys,rep(0,length(xs))), col="gray80", border=NA)
lines(xs, ys, lwd=5)

# the shaded area corresponds to the probability that y=1
plogis(0, location=-1.07, scale=1, lower.tail=FALSE)

# what would happen if -1.40 + 0.33 * x is equal to 0? then there is a 50%
# probability of y=1 (and analogously, a 50% probability of y=0)
plogis(0, location=0, scale=1, lower.tail=FALSE)
