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
res1 <- stan_glm(switch ~ dist, family=binomial(link="logit"), data=dat, refresh=0)
print(res1, digits=3)

# Figure 13.8a: histogram of distance to the nearest safe well
hist(dat$dist, breaks=50, xlab="Distance (in meters) to nearest safe well", main="")

# fit the logistic regression model using only 'dist100' as predictor
res1 <- stan_glm(switch ~ dist100, family=binomial(link="logit"), data=dat, refresh=0)
print(res1, digits=3)

## Graphing the fitted model

jitter_binary <- function(a, jitt=0.05)
   ifelse(a==0, runif(length(a), 0, jitt), runif(length(a), 1-jitt, 1))

# Figure 13.8b: graphical expression of the fitted logistic regression model
dat$switch_jitter <- jitter_binary(dat$switch)
plot(dat$dist100, dat$switch_jitter, pch=21, bg="gray", cex=0.5, bty="l",
     xlab="Distance (in 100 meters) to nearest safe well", ylab="Pr(Switching)")
curve(invlogit(coef(res1)[1] + coef(res1)[2]*x), lwd=3, add=TRUE)

# show the distribution of the dist100 variable within the two groups
plot(density(dat$dist100[dat$switch==1]), lwd=3, main="", bty="l", col="dodgerblue",
     xlab="Distance (in 100 meters) to nearest safe well")
lines(density(dat$dist100[dat$switch==0]), lwd=3, col="firebrick")
legend("topright", lwd=3, col=c("dodgerblue","firebrick"),
       legend=c("Switchers", "Non-Switchers"))

## Interpreting the logistic regression coefficients

# apply the inverse logit function to the intercept (to be precise, to the
# median of the posterior distribution of the intercept)
round(plogis(coef(res1)[[1]]), digits=2)

# to get a credible/percentile interval for this value, we need to obtain
# samples for the entire posterior distribution for this predicted value
pred <- posterior_epred(res1, newdata=data.frame(dist100=0))
round(apply(pred, 2, median), digits=2)
round(quantile(pred[,1], prob=c(.025, .975)), digits=2)

# use the divide-by-4 rule to get the maximum difference in probability of
# switching for a one-unit increase in x (distance in 100 meters)
coef(res1)[[2]] / 4

# extract the samples of the posterior distributions for the intercept and slope
post <- as.data.frame(res1)

# get the medians of the distributions
apply(post, 2, median)

# construct 95% percentile intervals for the two coefficients
round(apply(post, 2, quantile, prob=c(.025, .975)), digits=2)

# note: in the book, the interval for the slope is incorrectly reported to be
# [-0.73, -0.49], but it is actually [-0.82, -0.43]

# fit the logistic regression model with no predictors (only an intercept)
res0 <- stan_glm(switch ~ 1, family=binomial(link="logit"), data=dat, refresh=0)
print(res0, digits=2)
round(plogis(coef(res0)[[1]]), digits=2)

# compute the leave-one-out log scores for the two models
loo0 <- loo(res0)
loo1 <- loo(res1)
loo0
loo1

# compare the scores
loo_compare(loo0, loo1)

## Adding a second input variable

# Figure 13.9: histogram of arsenic levels in unsafe wells (those exceeding 0.5)
hist(dat$arsenic, breaks=50, xlab="Arsenic concentration in well water",
     main="", xlim=c(0,max(dat$arsenic)))

# add 'arsenic' as an additional predictor to the model
res2 <- stan_glm(switch ~ dist100 + arsenic, family=binomial(link="logit"),
                 data=dat, refresh=0)
print(res2, digits=2)

# use the divide-by-4 rule interpret the slopes
round(coef(res2)[2:3] / 4, digits=2)

# compute how the log odds changes for a one SD increase in the predictors
coef(res2)[2:3] * c(sd(dat$dist100), sd(dat$arsenic))

# apply the divide-by-4 rule to these values
round(coef(res2)[2:3] * c(sd(dat$dist100), sd(dat$arsenic)) / 4, digits=2)

# compute the leave-one-out log scores for this model
loo2 <- loo(res2)

# compare the scores
loo_compare(loo0, loo1, loo2)

## Comparing the coefficient estimates when adding a predictor

# examine the change in the slope for dist100 when arsenic is added to the model
coef(res1)
coef(res2)

## Graphing the fitted model with two predictors

# Figure 13.10a: probability of switching as a function of dist100 when
# arsenic level is equal to 0.5, 1, or 4
plot(dat$dist100, dat$switch_jitter, pch=21, bg="gray", cex=0.5, bty="l",
     xlab="Distance (in 100 meters) to nearest safe well", ylab="Pr(Switching)")
pred <- curve(invlogit(cbind(1, x, 0.5) %*% coef(res2)), add=TRUE, lwd=3)
text(pred$x[30]-0.10, pred$y[30], "arsenic = 0.5", pos=2)
pred <- curve(invlogit(cbind(1, x, 1.0) %*% coef(res2)), add=TRUE, lwd=3)
text(pred$x[30]+0.04, pred$y[30], "arsenic = 1.0", pos=4)
pred <- curve(invlogit(cbind(1, x, 4.0) %*% coef(res2)), add=TRUE, lwd=3)
text(pred$x[30]+0.04, pred$y[30], "arsenic = 4.0", pos=4)

# Figure 13.10b: probability of switching as a function of arsenic level when
# dist100 is equal to 0, 0.5, or 2
plot(dat$arsenic, dat$switch_jitter, pch=21, bg="gray", cex=0.5, bty="l",
     xlab="Arsenic concentration in well water", ylab="Pr(Switching)")
pred <- curve(invlogit(cbind(1, 0.0, x) %*% coef(res2)), add=TRUE, lwd=3)
text(pred$x[30]-0.10, pred$y[30], "dist100 = 0", pos=2)
pred <- curve(invlogit(cbind(1, 0.5, x) %*% coef(res2)), add=TRUE, lwd=3)
text(pred$x[30]+0.06, pred$y[30], "dist100 = 0.5", pos=4)
pred <- curve(invlogit(cbind(1, 2.0, x) %*% coef(res2)), add=TRUE, lwd=3)
text(pred$x[30]+0.06, pred$y[30], "dist100 = 2", pos=4)

## Add even more predictors

# add assoc and educ4 as additional predictors to the model
res3 <- stan_glm(switch ~ dist100 + arsenic + assoc + educ4,
                 family=binomial(link="logit"), data=dat, refresh=0)
print(res2, digits=2)
