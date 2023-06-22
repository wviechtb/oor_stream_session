############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-06-22
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 3.5 -
#
# last updated: 2023-06-22

############################################################################

### 3.5: Probability distributions

# draw a normal distribution with a mean of 63.7 and a standard deviation of 2.7
xs <- seq(55, 74, length=1000)
ys <- dnorm(xs, mean=63.7, sd=2.7)
plot(xs, ys, type="l", xlab="height (inches)", ylab="density", lwd=3)
abline(v=63.7, lty="dotted")

# shade in the area of the distribution where height is 60 inches or below
xs.sub <- seq(55, 60, length=1000)
ys.sub <- dnorm(xs.sub, mean=63.7, sd=2.7)
polygon(c(xs.sub,60,rev(xs.sub)), c(ys.sub,0,rep(0,1000)), col="lightgray", border=NA)
lines(xs, ys, lwd=3)

# calculate the size of the shaded area
pnorm(60, mean=63.7, sd=2.7)

# this is the probability of sampling a woman from the distribution whose
# height is 60 inches or shorter

# generate a random sample of the height of one million women from this distribution
set.seed(1234)
height <- rnorm(1000000, mean=63.7, sd=2.7)

# draw a histogram of the values
hist(height)

# increase the number of break points
hist(height, breaks=100)

# the proportion of women in the sample who are 60 inches or shorter
mean(height <= 60)

# redraw the normal distribution and shade in more areas
xs <- seq(55, 74, length=1000)
ys <- dnorm(xs, mean=63.7, sd=2.7)
plot(xs, ys, type="l", xlab="height (inches)", ylab="density", lwd=3)
xs.sub <- seq(63.7-3*2.7, 63.7+3*2.7, length=1000)
ys.sub <- dnorm(xs.sub, mean=63.7, sd=2.7)
polygon(c(xs.sub,63.7+3*2.7,rev(xs.sub)), c(ys.sub,0,rep(0,1000)), col="gray30", border=NA)
xs.sub <- seq(63.7-2*2.7, 63.7+2*2.7, length=1000)
ys.sub <- dnorm(xs.sub, mean=63.7, sd=2.7)
polygon(c(xs.sub,63.7+2*2.7,rev(xs.sub)), c(ys.sub,0,rep(0,1000)), col="gray60", border=NA)
xs.sub <- seq(63.7-1*2.7, 63.7+1*2.7, length=1000)
ys.sub <- dnorm(xs.sub, mean=63.7, sd=2.7)
polygon(c(xs.sub,63.7+1*2.7,rev(xs.sub)), c(ys.sub,0,rep(0,1000)), col="gray90", border=NA)
lines(xs, ys, lwd=3)
