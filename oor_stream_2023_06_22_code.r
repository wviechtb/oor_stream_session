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
xs <- seq(55, 60, length=1000)
ys <- dnorm(xs, mean=63.7, sd=2.7)
polygon(c(xs,60,rev(xs)), c(ys,0,rep(0,1000)), col="lightgray", border=NA)
lines(xs, ys, lwd=3)

# calculate the size of the shaded area
pnorm(60, mean=63.7, sd=2.7)

# this is the probability of sampling a woman from the distribution whose
# height is 60 inches or shorter

# generate a random sample of the height of one million women from this distribution
set.seed(1234)
height <- rnorm(1000000, mean=63.7, sd=2.7)

# the proportion of women in the sample who are 60 inches or shorter
mean(height <= 60)
