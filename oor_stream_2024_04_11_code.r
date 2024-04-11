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

generations <- 1000

means <- rep(NA, generations)
sds   <- rep(NA, generations)

for (i in 1:generations) {
   means[i] <- mean(heights)
   sds[i]   <- sd(heights)
   heights <- 30 + 0.5 * heights + rnorm(5000, mean=0, sd=2.3)
}

plot(1:generations, means, type="o", pch=21, bg="gray")
abline(h=60, lty="dotted")

# the means converge to intercept / (1 - slope), which is 60 here

plot(1:generations, sds, type="o", pch=21, bg="gray")
abline(h=60, lty="dotted")

# what's the equation for the value to which the SDs converge?
