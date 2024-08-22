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

# compute the predicted average weight for individuals who are 66 inches tall
coef(res)[[1]] + coef(res)[[2]] * 66