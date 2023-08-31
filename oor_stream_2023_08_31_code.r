############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-08-31
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 8.1 - ?
#
# last updated: 2023-08-31

############################################################################

### 8.1: R as a set of statistical tables

## normal distribution

# calculate the density of standard normal distribution for values between
# -4 and 4 and then plot the distribution (note: mean=0 and sd=1 are the
# defaults, so we could leave those out)
xs <- seq(-4, 4, length=10000)
ys <- dnorm(xs, mean=0, sd=1)
plot(xs, ys, type="l", lwd=2, bty="l")

# what proportion of the distribution falls below -1
pnorm(-1)

# so there is a ~16% probability of seeing a value between -infinity and -1
# when drawing a random value from a standard normal distribution

# shade in the corresponding area in the plot
xs <- seq(-4, -1, length=10000)
ys <- dnorm(xs, mean=0, sd=1)
polygon(c(xs,rev(xs)), c(ys,rep(0,10000)), col="lightgray")

# what proportion of the distribution falls above 2
pnorm(2, lower.tail=FALSE)

# shade in the corresponding area in the plot
xs <- seq(2, 4, length=10000)
ys <- dnorm(xs, mean=0, sd=1)
polygon(c(xs,rev(xs)), c(ys,rep(0,10000)), col="lightgray")

# find the value under which a proportion of 0.30 of the distribution falls
qnorm(.30)

# draw in the line that corresponds to this value
segments(qnorm(.30), 0, qnorm(.30), dnorm(qnorm(.30)))

# simulate 10 values from a standard normal distribution
rnorm(10)

## t-distribution

# first draw a standard normal distribution again
xs <- seq(-4, 4, length=10000)
ys <- dnorm(xs, mean=0, sd=1)
plot(xs, ys, type="l", bty="l", lty="dotted")

# calculate the density of a t-distribution with df=5 and add this to the plot
xs <- seq(-4, 4, length=10000)
ys <- dt(xs, df=5)
lines(xs, ys, type="l", lwd=2)

# add a legend
legend("topright", inset=.01, lty=c("dotted","solid"), lwd=c(1,2),
       legend=c("standard normal", "t-distribution (df=5)"))

# a simple animation showing how a t-distributions looks more and more like a
# standard normal when the degrees of freedom get large
for (i in 1:50) {
   xs <- seq(-4, 4, length=10000)
   ys <- dnorm(xs, mean=0, sd=1)
   plot(xs, ys, type="l", bty="l", lty="dotted")
   xs <- seq(-4, 4, length=10000)
   ys <- dt(xs, df=i)
   lines(xs, ys, type="l", lwd=2)
   legend("topright", inset=.01, lty=c("dotted","solid"), lwd=c(1,2),
          legend=c("standard normal", paste0("t-distribution (df=",i,")")))
   Sys.sleep(0.2)
}

# say you do an independent samples t-test (with df=20) and obtain a
# t-statistic of 2.23, then we can use pt() to compute the two-sided p-value
# as follows
2 * pt(2.23, df=20, lower.tail=FALSE)

# draw the corresponding t-distribution and shade in the tail areas
xs <- seq(-4, 4, length=10000)
ys <- dt(xs, df=20)
plot(xs, ys, type="l", lwd=2, bty="l")
xs <- seq(2.23, 4, length=10000)
ys <- dt(xs, df=20)
polygon(c(xs,rev(xs)), c(ys,rep(0,10000)), col="lightgray")
xs <- seq(-4, -2.23, length=10000)
ys <- dt(xs, df=20)
polygon(c(xs,rev(xs)), c(ys,rep(0,10000)), col="lightgray")

## chi-squared distribution

# calculate the density of a chi-squared distribution with df=2
xs <- seq(0, 10, length=10000)
ys <- dchisq(xs, df=2)
plot(xs, ys, type="l", lwd=2, bty="l")

# add lines for df=4 and df=6
ys <- dchisq(xs, df=4)
lines(xs, ys, type="l", lwd=2, lty="dashed")
ys <- dchisq(xs, df=6)
lines(xs, ys, type="l", lwd=2, lty="dotted")

# add legend
legend("topright", inset=.01, lty=c("solid","dashed","dotted"), lwd=2,
       legend=c("chi-squared (df=2)", "chi-squared (df=4)", "chi-squared (df=6)"))

# what area of a chi-square distribution with df=1 falls above 3.84
pchisq(3.84, df=1, lower.tail=FALSE)

## binomial distribution

# compute the probability of seeing 0, 1, ..., 10 tails when flipping a coin
# 10 times where the probability of a tail is 0.6 (the coin is NOT fair)
xs <- 0:10
ys <- dbinom(xs, size=10, prob=0.6)
ys

# for example, there is a ~25% chance that you will see 6 tails (and 4 heads)

# plot the distribution
plot(xs, ys, type="h")

# what is the probability of seeing 6 tails or fewer
pbinom(6, size=10, prob=0.6)

############################################################################

## 8.2: Examining the distribution of a set of data

# note: we are not using attach() for reasons discussed in the session on
# 2023-06-15 (briefly: using attach() can at times be confusing)

# copy the faithful dataset to dat
dat <- faithful

# examine the first 6 rows of the dataset
head(dat)

summary(dat$eruptions)
