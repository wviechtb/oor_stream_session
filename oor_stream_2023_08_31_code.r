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

# calculate the density of standard normal distribution for values between -4
# and 4 and then plot the distribution (note: mean=0 and sd=1 are the
# defaults, so we could leave those out)
xs <- seq(-4, 4, length=10000)
ys <- dnorm(xs, mean=0, sd=1)
plot(xs, ys, type="l", lwd=2, bty="l")

# what proportion of the distribution falls below -1
pnorm(-1)

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
