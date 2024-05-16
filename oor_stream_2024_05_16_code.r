############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-05-16
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 9.1 - ?
#
# last updated: 2024-05-16

############################################################################

### 9.1: Propagating uncertainty in inference using posterior simulations

# download the dataset for the example
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat", destfile="hibbs.dat")

# read in the data
dat <- read.table("hibbs.dat", header=TRUE)

# inspect the dataset
dat

# plot the data
plot(dat$growth, dat$vote, pch=21, bg="gray",
     xlab="Average recent growth in personal income",
     ylab="Incumbent party's vote share", bty="l", panel.first=grid())

# load the rstanarm package
library(rstanarm)

# fit a linear regression model
set.seed(1237)
res <- stan_glm(vote ~ growth, data=dat, refresh=0)
res

# extract the simulated draws from the posterior distributions for the
# intercept, slope, and error standard deviation as a data frame
post <- as.data.frame(res)
head(post)

# compute the median and mad values for the three columns
tab <- cbind(median = sapply(post, median), mad = sapply(post, mad))
tab

## Uncertainty in the regression coefficients and implied uncertainty in the regression line

# plot histograms and superimposed kernel density estimates of the three
# posterior distributions (similar to Figure 9.1)
par(mfrow=c(3,1))
hist(post[[1]], main="Intercept", freq=FALSE, breaks=80)
abline(v=median(post[[1]]), lwd=6)
arrows(tab[1,1] - 1*tab[1,2], par("usr")[4]*.40, tab[1,1] + 1*tab[1,2], par("usr")[4]*.40, code=3, length=0.15, lwd=2)
arrows(tab[1,1] - 2*tab[1,2], par("usr")[4]*.20, tab[1,1] + 2*tab[1,2], par("usr")[4]*.20, code=3, length=0.15, lwd=2)
lines(density(post[[1]]), lwd=3)
hist(post[[2]], main="Slope", freq=FALSE, breaks=80)
abline(v=median(post[[2]]), lwd=6)
arrows(tab[2,1] - 1*tab[2,2], par("usr")[4]*.40, tab[2,1] + 1*tab[2,2], par("usr")[4]*.40, code=3, length=0.15, lwd=2)
arrows(tab[2,1] - 2*tab[2,2], par("usr")[4]*.20, tab[2,1] + 2*tab[2,2], par("usr")[4]*.20, code=3, length=0.15, lwd=2)
lines(density(post[[2]]), lwd=3)
hist(post[[3]], main="Sigma", freq=FALSE, breaks=80)
abline(v=median(post[[3]]), lwd=6)
arrows(tab[3,1] - 1*tab[3,2], par("usr")[4]*.40, tab[3,1] + 1*tab[3,2], par("usr")[4]*.40, code=3, length=0.15, lwd=2)
arrows(tab[3,1] - 2*tab[3,2], par("usr")[4]*.20, tab[3,1] + 2*tab[3,2], par("usr")[4]*.20, code=3, length=0.15, lwd=2)
lines(density(post[[3]]), lwd=3)
par(mfrow=c(1,1))

# Figure 9.2(a)
plot(post[[1]], post[[2]], pch=19, cex=0.2, xlab="Intercept", ylab="Slope", bty="l")

# we can also plot the draws for all three parameters against each other
pairs(post, pch=19, cex=0.2)

# Figure 9.2(b) but also highlight the regression line based on the median intercept and slope
plot(dat$growth, dat$vote, type="n",
     xlab="Average recent growth in personal income",
     ylab="Incumbent party's vote share", bty="l")
apply(post[1:100,], 1, function(x) abline(a=x[1], b=x[2]))
abline(a=tab[1,1], b=tab[2,1], lwd=3)
points(dat$growth, dat$vote, pch=21, bg="black", col="white", lwd=3)
