############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-08-22
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 10.1 - ?
#
# last updated: 2024-08-22

############################################################################

### 10.1: Adding predictors to a model

## Starting with a binary predictor

# download the dataset (need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# load the rstanarm package
library(rstanarm)

# fit linear regression model predicting the kids' test score from the dummy
# variable mom_hs (1/0 = mom did or did not graduate from high-school) (note:
# we set the seed of the random number generator to make results fully
# reproducible)
set.seed(1234)
res <- stan_glm(kid_score ~ mom_hs, data=dat, refresh=0)
res

# Figure 10.1
plot(jitter(kid_score, amount=0.5) ~ jitter(mom_hs, amount=0.05), data=dat, pch=19, xlim=c(-0.2, 1.2),
     xlab="Mother completed high school", xaxt="n", ylab="Child test score", cex=0.5)
axis(side=1, at=c(0,1), labels=c("No (0)", "Yes (1)"))
abline(res, lwd=4)

# compute the observed means of the two groups
means <- with(dat, by(kid_score, mom_hs, mean))
means

# the slope of the regression model is really the difference between the two groups
means[2] - means[1]

# also add the observed means of the two groups as red points to the figure
points(c(0,1), means, pch=19, cex=2, col="red")

## A single continuous predictor

# fit linear regression model predicting the kids' test score from the mothers' IQ
res <- stan_glm(kid_score ~ mom_iq, data=dat, refresh=0)
res

# Figure 10.2
plot(jitter(kid_score, amount=0.5) ~ jitter(mom_iq, amount=0.5), data=dat, pch=19,
     xlab="Mother IQ score", ylab="Child test score", cex=0.5)
abline(res, lwd=4)

## Including both predictors

# now include both predictors in the model
res <- stan_glm(kid_score ~ mom_hs + mom_iq, data=dat, refresh=0)
res

# note: the results we obtain differ slightly from those given in the book
# because of the randomness when sampling a finite number of values from the
# posterior distributions of the model parameters

# Figure 10.3
plot(jitter(kid_score, amount=0.5) ~ jitter(mom_iq, amount=0.5), data=dat, pch=19,
     col=ifelse(mom_hs==1, "gray", "black"), xlab="Mother IQ score",
     ylab="Child test score", cex=0.5)
abline(a=coef(res)[1],                b=coef(res)[3], lwd=4)
abline(a=coef(res)[1] + coef(res)[2], b=coef(res)[3], lwd=4, col="gray")

############################################################################

### 10.3: Interactions

res <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=dat, refresh=0)
res

# Figure 10.4a
plot(jitter(kid_score, amount=0.5) ~ jitter(mom_iq, amount=0.5), data=dat, pch=19,
     col=ifelse(mom_hs==1, "gray", "black"), xlab="Mother IQ score",
     ylab="Child test score", cex=0.5)
abline(a=coef(res)[1],                b=coef(res)[3],                lwd=4)
abline(a=coef(res)[1] + coef(res)[2], b=coef(res)[3] + coef(res)[4], lwd=4, col="gray")

# another way to draw the regression lines is to compute predicted values for
# mom IQ scores between let's say 70 and 140 when mom_hs is equal to 0 or 1
# and then add the corresponding lines to the plot
iqs <- 70:140
pred <- coef(res)[1] + coef(res)[2]*0 + coef(res)[3]*iqs + coef(res)[4]*0*iqs
lines(iqs, pred, lwd=4)
pred <- coef(res)[1] + coef(res)[2]*1 + coef(res)[3]*iqs + coef(res)[4]*1*iqs
lines(iqs, pred, lwd=4, col="gray")

# Figure 10.4b
plot(jitter(kid_score, amount=0.5) ~ jitter(mom_iq, amount=0.5), data=dat, pch=19,
     col=ifelse(mom_hs==1, "gray", "black"), xlab="Mother IQ score",
     ylab="Child test score", cex=0.5, xlim=c(0,150), ylim=c(-20,150))
abline(a=coef(res)[1],                b=coef(res)[3],                lwd=4)
abline(a=coef(res)[1] + coef(res)[2], b=coef(res)[3] + coef(res)[4], lwd=4, col="gray")

############################################################################

### 10.4: Indicator variables

# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv", destfile="earnings.csv")

# read in the dataset
dat <- read.csv("earnings.csv")

# inspect the dataset
head(dat)

# fit linear regression model predicting weight from height
res <- stan_glm(weight ~ height, data=dat, refresh=0)
res

# compute the predicted average weight for persons who are 66 inches tall
coef(res)[[1]] + coef(res)[[2]] * 66

# recall from chapter 9: we can predict the average weight of persons who are
# 66 inches tall or the weight of individual persons who are 66 inches tall;
# using posterior_predict(), we get the latter (or more precisely, we get
# draws from the posterior distribution, which we can then summarize with
# mean() and sd())
newdat <- data.frame(height=66)
pred <- posterior_predict(res, newdata=newdat)
mean(pred)
sd(pred)

## Centering a predictor

# center the height variable at 66 inches and refit the model using this
# centered predictor; then the intercept of the model directly reflects the
# estimated average weight of individuals who are 66 inches tall
dat$c_height <- dat$height - 66
res <- stan_glm(weight ~ c_height, data=dat, refresh=0)
res

## Including a binary variable in a regression

# include the dummy variable male in the model
res <- stan_glm(weight ~ c_height + male, data=dat, refresh=0)
res

# computed the predicted average weight of 70-inch tall women
coef(res)[[1]] + coef(res)[[2]]*4 + coef(res)[[3]]*0

# use posterior_predict() to make predictions of the weight of individual
# women who are 70 inches tall
newdat <- data.frame(c_height=4, male=0)
pred <- posterior_predict(res, newdata=newdat)
mean(pred)
sd(pred)

# do the same prediction for individual men
newdat <- data.frame(c_height=4, male=1)
pred <- posterior_predict(res, newdata=newdat)
mean(pred)
sd(pred)

## Using indicator variables for multiple levels of a categorical predictor

# now add the 4-level categorical ethnicity variable to the model
res <- stan_glm(weight ~ c_height + male + factor(ethnicity), data=dat, refresh=0)
res

## Changing the baseline factor level

# could use factor() and specify the order of all levels as shown in the book
dat$eth <- factor(dat$ethnicity), levels=c("White", "Black", "Hispanic", "Other"))

# or we could use the relevel() function to make 'White' the baseline category
# without having to specify the order of the other levels (which will be in
# alphabetical order anyway)
dat$eth <- relevel(factor(dat$ethnicity), ref="White")

# refit the model
res <- stan_glm(weight ~ c_height + male + eth, data=dat, refresh=0)
res

############################################################################

### 10.5: Formulating paired or blocked designs as a regression problem

## Completely randomized experiment

# create a dataset corresponding to this discussion
dat <- data.frame(trt=rep(c(0,1), times=c(22,26)))
dat$outcome <- round(4 + dat$trt * 2 + rnorm(nrow(dat)))
dat

# compute the means, SDs, and group sizes for the control and treatment group
means <- with(dat, by(outcome, trt, mean))
sds   <- with(dat, by(outcome, trt, sd))
ns    <- with(dat, by(outcome, trt, length))

# compute the mean difference and corresponding standard error
means[[2]] - means[[1]]
sqrt(sds[[2]]^2 / ns[[2]] + sds[[1]]^2 / ns[[1]])

# fit the corresponding regression model
res <- stan_glm(outcome ~ trt, data=dat, refresh=0)
res

## Paired design

# create a dataset corresponding to this discussion
dat <- data.frame(person1 = round(rnorm(20, mean=6, sd=1)),
                  person2 = round(rnorm(20, mean=4, sd=1)))
dat

# note: people in the person1 column are in the treatment group, people in the
# person2 column are in the control group

# compute the difference within pairs
dat$z <- dat$person1 - dat$person2

# compute the mean of the differences and the corresponding standard error
mean(dat$z)
sd(dat$z) / sqrt(nrow(dat))

# remove the z variable
dat$z <- NULL

# restructure the data into a long format
dat <- reshape(dat, direction="long", varying=c("person1","person2"),
               v.names="outcome", timevar="trt", times=c(1,0), idvar="pair")
dat <- sort_by(dat, ~ pair)
rownames(dat) <- NULL
dat

# fit the corresponding regression model
res <- stan_glm(outcome ~ trt + factor(pair), data=dat, refresh=0)
print(res, digits=2)

## Block design

# a common situation where this type of design arises is in a multicenter
# study, where individuals are randomized to a treatment versus control
# condition within each center

# create a dataset corresponding to this discussion
dat <- data.frame(center = rep(1:6, times=c(6,18,15,9,23,12)))
