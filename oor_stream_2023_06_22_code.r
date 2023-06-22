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

# redraw the normal distribution and shade in more areas (for 1, 2, and 3
# standard deviations around the mean)
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

# size of the area mu-sigma to mu+sigma (one SD below to one SD above the mean)
pnorm(63.7+1*2.7, mean=63.7, sd=2.7) - pnorm(63.7-1*2.7, mean=63.7, sd=2.7)

# so this is the probability of sampling a women from the distribution whose
# height is between one SD below the mean to one SD above the mean

# the proportion of women in the sample whose height falls within this range
mean(height >= 63.7-1*2.7 & height <= 63.7+1*2.7)

# size of the area mu-2*sigma to mu+2*sigma (two SDs below to two SDs above the mean)
pnorm(63.7+2*2.7, mean=63.7, sd=2.7) - pnorm(63.7-2*2.7, mean=63.7, sd=2.7)

# size of the area mu-3*sigma to mu+3*sigma (three SDs below to three SDs above the mean)
pnorm(63.7+3*2.7, mean=63.7, sd=2.7) - pnorm(63.7-3*2.7, mean=63.7, sd=2.7)

# draw the normal distributions for the women and men in the same plot
xs <- seq(55, 74, length=1000)
ys <- dnorm(xs, mean=63.7, sd=2.7)
plot(xs, ys, type="l", xlab="height (inches)", ylab="density", lwd=3,
     xlim=c(55,80), col="firebrick")
abline(v=63.7, lty="dotted")
xs <- seq(60, 80, length=1000)
ys <- dnorm(xs, mean=69.1, sd=2.9)
lines(xs, ys, lwd=3, col="dodgerblue")
abline(v=69.1, lty="dotted")
legend("topright", inset=.02, col=c("firebrick","dodgerblue"), lty=1,
       legend=c("women","men"), lwd=3)

# draw the density for the 50/50 mixture of the two distributions
xs <- seq(55, 80, length=1000)
ys.w <- dnorm(xs, mean=63.7, sd=2.7)
ys.m <- dnorm(xs, mean=69.1, sd=2.9)
ys <- 0.5 * ys.w + 0.5 * ys.m
plot(xs, ys, type="l", xlab="height (inches)", ylab="density", lwd=3)

# to illustrate the CLT, let's consider ...
x001 <- sample(c(0,1), 100000, replace=TRUE)
x002 <- sample(c(0,1), 100000, replace=TRUE)
# ...
x350 <- sample(c(0,1), 100000, replace=TRUE)

# instead of doing the above a 100 times, we can use replicate()
set.seed(1234)
X <- replicate(350, sample(c(0,1), 100000, replace=TRUE))

# X is a matrix with 1000000 rows and 350 columns; the columns are the 350
# variables representing the presence/absence of genetics factors that
# influence the height of a person

# take the sum across these 350 variables (the row sums)
height <- rowSums(X)

# draw a histogram of the resulting values for the 100000 people
hist(height, breaks=100)
