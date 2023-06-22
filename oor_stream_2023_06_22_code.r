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

## Normal distribution; mean and standard deviation
# https://en.wikipedia.org/wiki/Normal_distribution

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

# X is a matrix with 100000 rows and 350 columns; the columns are the 350
# variables representing the presence/absence of genetics factors that
# influence the height of a person

# take the sum across these 350 variables (the row sums)
height <- rowSums(X)

# draw a histogram of the resulting values for the 100000 people
hist(height, breaks=100)

# the mean, variance, and SD of the height values
mean(height)
var(height)
sd(height)

# the mean of a variable that takes on with 50% change the value 1 and with
# 50% change the value 0 is 0.5; we summed up 350 of such variables, so the
# true mean of the height variable we generated above must be 350 times this
# mean
350 * 0.5

# the variance of each of the variables in X is equal to 0.25
((0-0.5)^2 + (1-0.5)^2) / 2

# the variance of the height variable then is 350 times this variance
350 * 0.25

# and hence the SD of the height variable is the square root of that
sqrt(350 * 0.25)

## Linear transformations

# transform the height in centimeters into the height in inches and draw the
# histogram of these values (still a normal distribution)
hist(height / 2.54)

# simulate the difference of the mean height of 100 men and the mean height of 100 women
mean(rnorm(100, mean=69.1, sd=2.9)) - mean(rnorm(100, mean=63.7, sd=2.7))

# replicate this process 100000 times
mdiff <- replicate(100000, mean(rnorm(100, mean=69.1, sd=2.9)) -
                           mean(rnorm(100, mean=63.7, sd=2.7)))

# histogram of the resulting values
hist(mdiff, breaks=100)

# the mean and SD of the resulting values
mean(mdiff)
sd(mdiff)

## Mean and standard deviation of the sum of correlated random variables

# say we measure people twice on the same variable, once before and once after
# some kind of treatment
set.seed(1234)
pretest  <- rnorm(100, mean=100, sd=15)
posttest <- pretest + rnorm(100, mean=12, sd=10)

# create a scatterplot of the two variables
plot(pretest, posttest, pch=21, bg="gray", xlab="Pre-test", ylab="Post-test")

# add the diagonal line for when pretest = posttest
abline(a=0, b=1)

# correlation between the two variables
cor(pretest, posttest)

# compute the sum of the two scores
sumscore <- posttest + pretest

# compute the sum of the means and the mean of the sum scores
mean(pretest) + mean(posttest)
mean(sumscore)

# compute the SD of the sum scores and compare this to the formula in the book
sd(sumscore)
sqrt(var(pretest) + var(posttest) + 2*cor(pretest, posttest)*sd(pretest)*sd(posttest))

# compute the change scores
change <- posttest - pretest

# compute the difference between the means and the mean of the change scores
mean(posttest) - mean(pretest)
mean(change)

# compute the SD of the change scores and compare this to the formula in the book
sd(change)
sqrt(var(pretest) + var(posttest) - 2*cor(pretest, posttest)*sd(pretest)*sd(posttest))

## Lognormal distribution
# https://en.wikipedia.org/wiki/Log-normal_distribution

# simulate the log weight of 1000000 men
logweight <- rnorm(1000000, mean=5.13, sd=0.17)

# histogram of these values
hist(logweight, breaks=100)

# exponentiate these values to get the weight (in pounds) of these people
weight <- exp(logweight)

# histogram of the weight values
hist(weight, breaks=100)

# exponentiate the mean and SD of the logweight values
exp(mean(logweight))
exp(sd(logweight))

# note: these are not the same as taking the mean and SD of the weight values
mean(weight)
sd(weight)

# we can estimate the mean weight from the mean and SD logweight values
exp(mean(logweight) + var(logweight)/2)

# and we can estimate the SD weight from the mean and SD logweight values
sqrt((exp(var(logweight)) - 1) * exp(2*mean(logweight) + var(logweight)))

#
exp(median(logweight))
median(weight)
