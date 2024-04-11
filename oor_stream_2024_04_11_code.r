############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-04-11
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 6.5 - ?
#
# last updated: 2024-04-11

############################################################################

### 6.5: The paradox of regression to the mean

# install the rstanarm package (need to do this once)
#install.packages("rstanarm")

# load the rstanarm package
library(rstanarm)

# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/PearsonLee/data/Heights.txt", destfile="heights.txt")

# read in the data
dat <- read.table("heights.txt", header=TRUE)

# inspect the dataset
head(dat)

# Figure 6.3a: mother's height versus daughter's height (with jittering to
# avoid points that overlap)
plot(jitter(daughter_height, amount=0.5) ~ jitter(mother_height, amount=0.5),
     data=dat, pch=19, cex=0.2, xlab="Mother's height (inches)",
     ylab="Adult daughter's height (inches)", bty="l")
grid()

# fit a simple linear regression model predicting the daughter's height based
# on the mother's height
res <- stan_glm(daughter_height ~ mother_height, data=dat)
res

# extract the coefficients (rounded to two decimal places)
round(coef(res), digits=2)

# add the regression line to the plot
abline(res, lwd=5)

# also add a diagonal line with a slope of 1
abline(a=0, b=1, lwd=5, lty="dotted")

# compare the variance of mothers' and daughters' heights
var(dat$mother_height)
var(dat$daughter_height)

# so the variation in daughers' heights is still as large (or even a bit
# larger) than the variation in mothers' heights

############################################################################

set.seed(1234)

heights <- rnorm(5000, mean=62.5, sd=2.3)

generations <- 100

means <- rep(NA, generations)
sds   <- rep(NA, generations)

for (i in 1:generations) {
   means[i] <- mean(heights)
   sds[i]   <- sd(heights)
   heights <- 30 + 0.5 * heights + rnorm(5000, mean=0, sd=2.3)
}

plot(1:generations, means, type="o", pch=21, bg="gray")

# it can be shown that the means converge to intercept / (1 - slope)
30 / (1 - 0.5)
abline(h = 30 / (1 - 0.5), lty="dotted")

plot(1:generations, sds, type="o", pch=21, bg="gray")

# it can be shown that the SDs converge to sigma / sqrt(1 - slope^2)
2.3 / sqrt(1 - 0.5^2)
abline(h = 2.3 / sqrt(1 - 0.5^2), lty="dotted")

############################################################################

## How regression to the mean can confuse people about causal inference;
## demonstration using fake data

set.seed(1234)
ability <- rnorm(1000, mean=50, sd=10)
midterm <- ability + rnorm(1000, mean=0, sd=10)
final   <- ability + rnorm(1000, mean=0, sd=10)

dat <- data.frame(midterm, final)
rm(ability, midterm, final)

res <- stan_glm(final ~ midterm, data=dat)
res

plot(final ~ midterm, data=dat, pch=21, bg="gray",
     xlab="Midterm exam score", ylab="Final exam score")
grid()
abline(res, lwd=5)
abline(a=0, b=1, lwd=5, lty="dotted")

############################################################################

### 7.1: Example: predicting presidential vote share from the economy

# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat", destfile="hibbs.dat")

# read in the data
dat <- read.table("hibbs.dat", header=TRUE)

# look at the data
dat

# plot growth on the x-axis versus vote on the y-axis (Figure 1.1a)
plot(vote ~ growth, data=dat, xlab="Average recent growth in personal income",
     ylab="Incumbent party's vote share", pch=NA, xaxt="n", yaxt="n",
     xlim=c(-0.5,4.5), ylim=c(43,62), bty="l")
abline(h=50, col="gray")
with(dat, text(growth, vote, year, pos=4))
axis(side=1, at=0:4, labels=paste0(0:4, "%"))
axis(side=2, at=c(45,50,55,60), labels=paste0(c(45,50,55,60), "%"))
title("Forecasting the election from the economy")

# fit regression model (using the method of least squares)
res <- lm(vote ~ growth, data=dat)
summary(res)

# plot growth versus vote and add the regression line (Figure 1.1b)
plot(vote ~ growth, data=dat, xlab="Average recent growth in personal income",
     ylab="Incumbent party's vote share", pch=19, xaxt="n", yaxt="n",
     xlim=c(-0.5,4.5), ylim=c(43,62), bty="l")
abline(h=50, col="gray")
points(vote ~ growth, data=dat, pch=19)
axis(side=1, at=0:4, labels=paste0(0:4, "%"))
axis(side=2, at=c(45,50,55,60), labels=paste0(c(45,50,55,60), "%"))
title("Data and linear fit")
abline(res, lwd=3)
text(3, coef(res)[1] + coef(res)[2]*3, pos=4, offset=2,
     paste0("y = ", round(coef(res)[1], 1), " + ", round(coef(res)[2], 1), " x"))

# install the rstanarm package
#install.packages("rstanarm")

# load the rstanarm package
library(rstanarm)

# fit the model using stan_glm() (first setting the seed of the random number
# generator to make the results fully reproducible)
set.seed(1234)
res <- stan_glm(vote ~ growth, data=hibbs)
res

# get more detailed information about the fitted model
summary(res)

# extract the posterior samples
post <- as.data.frame(res)
head(post)

# histogram of the posterior distribution for the slope
hist(post$growth, xlab="Slope", main="")

# median of the posterior distribution for the slope
median(post$growth)

# 95% credible interval
quantile(post$growth, probs=c(.025,.975))
