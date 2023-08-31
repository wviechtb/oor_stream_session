############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-08-31
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 8.1 - 8.2
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

# read the help file for the dataset
help(faithful)

# examine the first 6 rows of the dataset
head(dat)

# compute some summary statistics for the eruptions variable
summary(dat$eruptions)
fivenum(dat$eruptions)

# look at the raw data for the eruptions variable rounded to two decimal places
round(dat$eruptions, digits=2)

# create a stem-and-leaf plot for the eruptions variable
stem(dat$eruptions, scale=2)

# there is one 1.60 in the dataset, one 1.67, one 1.70, one 1.73, six 1.75s,
# two 1.78s, and so on; so the step-and-leaf plot gives us the entire dataset
# (rounded to two decimal places) and it also shows us something about the
# distribution of the variable (i.e., there are actually two peaks, one where
# the eruption length is around 1.8 minutes, and another peak where the
# eruption time is around 4.5 minutes)

# if we don't adjust the scale argument and leave it at the default, then for
# this dataset, stem() groups the 1.6s and 1.7s together, the 1.8s and 1.9s,
# and so on
stem(dat$eruptions)

# draw a histogram for this variable
hist(dat$eruptions, col="gray30")

# increase the number of bins
hist(dat$eruptions, breaks=30, col="gray30")

# we can also specify the exact position of the break points
hist(dat$eruptions, breaks=seq(1,6,by=0.125), col="gray30")

# adjust the x-axis label and remove the (superfluous) title
hist(dat$eruptions, breaks=seq(1,6,by=0.125),
     xlab="Eruption Time (in minutes)", main="", col="gray30")

# change the y-axis from frequencies to densities
hist(dat$eruptions, breaks=seq(1,6,by=0.125), freq=FALSE,
     xlab="Eruption Time (in minutes)", main="", col="gray30")

# use density() to obtain a 'kernel density estimate' of the distribution of
# the eruptions variable (using the SJ rule for selecting the bandwidth)
lines(density(dat$eruptions, bw="SJ"), lwd=2)

# put tick marks below the histogram at the location of the eruption times
rug(dat$eruptions)

# keep only those rows of the dataset where eruptions > 3
dat <- dat[dat$eruptions > 3,]

# create a histogram of these data and superimpose a kernel density estimate
hist(dat$eruptions, breaks=seq(3,5.5,by=0.1), freq=FALSE,
     xlab="Eruption Time (in minutes)", main="")
lines(density(dat$eruptions, bw="SJ"), lwd=2)

# superimpose the density from a normal distribution with a mean and SD of
# that of the actual data
xs <- seq(3.0, 5.5, by=0.01)
ys <- dnorm(xs, mean=mean(dat$eruptions), sd=sd(dat$eruptions))
lines(xs, ys, lty="dotted", lwd=2)

# plot the empirical cumulative distribution function (ecdf)
plot(ecdf(dat$eruptions), do.points=TRUE, verticals=TRUE, lwd=2,
     xlab="Eruption Time (in minutes)", main="", col.01line=NA, bty="l")

# superimpose the cumulative distribution function from a normal distribution
# with a mean and SD of that of the actual data on top of the ecdf
xs <- seq(3.0, 5.5, by=0.01)
lines(xs, pnorm(xs, mean=mean(dat$eruptions), sd=sd(dat$eruptions)), lty="dotted", lwd=2)

# draw a Q-Q normal plot for the eruptions variable
qqnorm(dat$eruptions, pch=21, bg="lightgray")
qqline(dat$eruptions, lwd=3)

# https://en.wikipedia.org/wiki/Q%E2%80%93Q_plot

# ...
x <- rt(1000, df=5)
qqnorm(x, pch=21, bg="lightgray")
qqline(x, lwd=3)

# ...
x <- rchisq(1000, df=10)
qqnorm(x, pch=21, bg="lightgray")
qqline(x, lwd=3)

# carry out a Shapiro-Wilk test for the eruptions variable
# https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test
shapiro.test(dat$eruptions)

# carry out a Kolmogorov-Smirnov test test for the eruptions variable
# https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test
ks.test(dat$eruptions, pnorm, mean=mean(dat$eruptions), sd=sd(dat$eruptions))

# https://en.wikipedia.org/wiki/Normality_test

############################################################################
