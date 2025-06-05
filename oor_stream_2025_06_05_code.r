############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-06-05
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 13.7 - ?
#
# last updated: 2025-06-05

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### 13.7: Building a logistic regression model: wells in Bangladesh

# download the dataset if it doesn't already exist
if (!file.exists("wells.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/Arsenic/data/wells.csv", destfile="wells.csv")

# read in the dataset
dat <- read.csv("wells.csv")

# inspect the first six rows of the dataset
head(dat)

## Logistic regression with just one predictor

# fit the logistic regression model using only 'dist' as predictor
res <- stan_glm(switch ~ dist, family=binomial(link="logit"), data=dat, refresh=0)
print(res, digits=3)

# Figure 13.8a: histogram of distance to the nearest safe well
hist(dat$dist, breaks=50, xlab="Distance (in meters) to nearest safe well", main="")

# fit the logistic regression model using only 'dist100' as predictor
res <- stan_glm(switch ~ dist100, family=binomial(link="logit"), data=dat, refresh=0)
print(res, digits=3)

## Graphing the fitted model

jitter_binary <- function(a, jitt=0.05)
   ifelse(a==0, runif(length(a), 0, jitt), runif(length(a), 1-jitt, 1))

# Figure 13.8b: graphical expression of the fitted logistic regression model
dat$switch_jitter <- jitter_binary(dat$switch)
plot(dat$dist100, dat$switch_jitter, pch=21, bg="gray", cex=0.5, bty="l",
     xlab="Distance (in 100 meters) to nearest safe well", ylab="Pr(Switching)")
curve(invlogit(coef(res)[1] + coef(res)[2]*x), lwd=3, add=TRUE)

# show the distribution of the dist100 variable within the two groups
plot(density(dat$dist100[dat$switch==1]), lwd=3, main="", bty="l", col="dodgerblue",
     xlab="Distance (in 100 meters) to nearest safe well")
lines(density(dat$dist100[dat$switch==0]), lwd=3, col="firebrick")
legend("topright", lwd=3, col=c("dodgerblue","firebrick"),
       legend=c("Switchers", "Non-Switchers"))

## Interpreting the logistic regression coefficients

# apply the inverse logit function to the intercept (to be precise, to the
# median of the posterior distribution of the intercept)
round(plogis(coef(res)[[1]]), digits=2)

# to get a credible/percentile interval for this value, we need to obtain
# samples for the entire posterior distribution for this predicted value
pred <- posterior_epred(res, newdata=data.frame(dist100=0))
round(apply(pred, 2, median), digits=2)
round(quantile(pred[,1], prob=c(.025, .975)), digits=2)

# use the divide-by-4 rule to get the maximum difference in probability of
# switching for a one-unit increase in x (distance in 100 meters)
coef(res)[[2]] / 4

