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

# add assoc and educ as additional predictors to the model
res3 <- stan_glm(switch ~ dist100 + arsenic + assoc + educ,
                 family=binomial(link="logit"), data=dat, refresh=0)
print(res3, digits=2)

# compute the leave-one-out log scores for this model
loo3 <- loo(res3)

# compare the score for models res2 and res3
loo_compare(loo2, loo3)

# illustrate how the probability of switching depends on each predictor
# (holding the other predictors constant at their mean)
par(mfrow=c(2,2))
plot(dat$dist100, dat$switch_jitter, pch=21, bg="gray", cex=0.5, bty="l",
     xlab="Distance (in 100 meters) to nearest safe well", ylab="Pr(Switching)")
curve(invlogit(cbind(1, x, mean(dat$arsenic), mean(dat$assoc), mean(dat$educ)) %*% coef(res3)), add=TRUE, lwd=3)
plot(dat$arsenic, dat$switch_jitter, pch=21, bg="gray", cex=0.5, bty="l",
     xlab="Arsenic concentration in well water", ylab="Pr(Switching)")
curve(invlogit(cbind(1, mean(dat$dist100), x, mean(dat$assoc), mean(dat$educ)) %*% coef(res3)), add=TRUE, lwd=3)
plot(jitter(dat$assoc, amount=0.04), dat$switch_jitter, pch=21, bg="gray", cex=0.5, bty="l",
     xlab="Member(s) of household participate in community organizations", ylab="Pr(Switching)")
curve(invlogit(cbind(1, mean(dat$dist100), mean(dat$arsenic), x, mean(dat$educ)) %*% coef(res3)), add=TRUE, lwd=3)
plot(jitter(dat$educ, amount=0.2), dat$switch_jitter, pch=21, bg="gray", cex=0.5, bty="l",
     xlab="Years of education (head of household)", ylab="Pr(Switching)")
curve(invlogit(cbind(1, mean(dat$dist100), mean(dat$arsenic), mean(dat$assoc), x) %*% coef(res3)), add=TRUE, lwd=3)
par(mfrow=c(1,1))

# the emmeans package can also be used for creating such plots

# install.packages(emmeans)
library(emmeans)

plot(dat$dist100, dat$switch_jitter, pch=21, bg="gray", cex=0.5, bty="l",
     xlab="Distance (in 100 meters) to nearest safe well", ylab="Pr(Switching)")
curve(invlogit(cbind(1, x, mean(dat$arsenic), mean(dat$assoc), mean(dat$educ)) %*% coef(res3)), add=TRUE, lwd=3)

# compute the predicted values using emmeans() (note: all other variables are
# automatically held constant at their means)
pred <- emmeans(res3, specs = ~ dist100, at=list(dist100=seq(0,3.5,length.out=100)))
pred <- summary(pred, type="response")
head(pred)
lines(pred$dist100, pred$prob, col="red", lwd=3)
lines(pred$dist100, pred$lower.HPD, col="red", lwd=3, lty="dotted")
lines(pred$dist100, pred$upper.HPD, col="red", lwd=3, lty="dotted")

############################################################################

### 14.1: Graphing logistic regression and binary data

# simulate data as described (and based on the code given on the book website)
set.seed(1245)
n <- 50
a <- 2
b <- 3
x_mean <- -a/b
x_sd <- 4/b
dat <- data.frame(x = rnorm(n, mean=x_mean, sd=x_sd))
dat$y <- rbinom(n, 1, invlogit(a + b*dat$x))
head(dat)

# fit the logistic regression model
res <- stan_glm(y ~ x, family=binomial(link="logit"), data=dat, refresh=0)
print(res, digits=2)

# Figure 14.1a: plot of x versus y and the curve showing the predicted
# probability of y=1 as a function of x based on the model and the true model
plot(dat$x, dat$y, pch=19, cex=0.8, bty="l", xlab="x", ylab="y")
curve(invlogit(a + b*x), lwd=2, add=TRUE)
curve(invlogit(coef(res)[1] + coef(res)[2]*x), lwd=2, lty="dashed", add=TRUE)

# created a binned version of x
k <- 5
bins <- as.numeric(cut(dat$x, k))

# Figure 14.1b: plot x versus y and the proportions of y=1 within each bin
# (also add the line based on the fitted model)
plot(dat$x, dat$y, pch=19, cex=0.8, bty="l", xlab="x", ylab="y", col="darkgray")
props <- prop.table(table(bins, dat$y), margin=1)
props
curve(invlogit(coef(res)[1] + coef(res)[2]*x), lwd=1, lty="dashed", add=TRUE)
lines(by(dat$x, bins, mean), props[,"1"], pch=21, bg="white", cex=1.2, type="o")

# simulate data as described (and based on the code given on the book website)
set.seed(1245)
n <- 100
a <- 2
b1 <- 3
b2 <- 4
dat <- data.frame(x1 = rnorm(n, mean= 0,   sd=0.4),
                  x2 = rnorm(n, mean=-0.5, sd=0.4))
dat$y <- rbinom(n, 1, invlogit(a + b1*dat$x1 + b2*dat$x2))
head(dat)

# Figure 14.2: data simulated from a logistic regression model with two predictors
plot(dat$x1, dat$x2, pch=ifelse(dat$y == 1, 21, 19), xlab="x1", ylab="x2", bty="l")
legend("topright", pch=c(21,19), legend=c("y=1", "y=0"))

# fit the logistic regression model
res <- stan_glm(y ~ x1 + x2, family=binomial(link="logit"), data=dat, refresh=0)
print(res, digits=2)

# logit(Pr(y=1)) = 2.4 + 3.5*x1 + 5.00*x2
#
# when logit(Pr(y=1)) = 0, then Pr(y=1) is equal to 0.5
plogis(0)

# so we solve 2.4 + 3.5*x1 + 5.00*x2 = 0 for x2, which yields:
# x2 = -2.4/5.00 - 3.5/5.00*x1
# this defines the line where Pr(y=1) is equal to 0.5
abline(a=-2.4/5.00, b=-3.5/5.00, lwd=2)

# now we want plogis(2.4 + 3.5*x1 + 5.00*x2) = 0.1
# so 2.4 + 3.5*x1 + 5.00*x2 = qlogis(0.1)
# now solve this again for x2:
# x2 = (qlogis(0.1) - 2.4)/5.00 - 3.5/5.00*x1
# this defines the line where Pr(y=1) is equal to 0.1
abline(a=(qlogis(0.1) - 2.4)/5.00, b=-3.5/5.00, lwd=2, lty="dotted")

# this defines the line where Pr(y=1) is equal to 0.9
abline(a=(qlogis(0.9) - 2.4)/5.00, b=-3.5/5.00, lwd=2, lty="dotted")
