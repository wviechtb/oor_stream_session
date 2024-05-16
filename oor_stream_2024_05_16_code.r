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
     ylab="Incumbent party's vote share", bty="l",
      panel.first=grid())

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
cbind(median = sapply(post, median), mad = sapply(post, mad))
