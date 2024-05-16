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

## Uncertainty in the regression coefficients and implied uncertainty in the
## regression line

# plot histograms and superimposed kernel density estimates of the three
# posterior distributions (similar to Figure 9.1)
par(mfrow=c(3,1))
titles <- c("Intercept", "Slope", "Sigma")
for (i in 1:3) {
   hist(post[[i]], main=titles[i], freq=FALSE, breaks=80)
   abline(v=median(post[[i]]), lwd=6)
   arrows(tab[i,1] - 1*tab[i,2], par("usr")[4]*.40, tab[i,1] + 1*tab[i,2], par("usr")[4]*.40, code=3, length=0.15, lwd=2)
   arrows(tab[i,1] - 2*tab[i,2], par("usr")[4]*.20, tab[i,1] + 2*tab[i,2], par("usr")[4]*.20, code=3, length=0.15, lwd=2)
   lines(density(post[[i]]), lwd=3)
}
par(mfrow=c(1,1))

# Figure 9.2(a)
plot(post[[1]], post[[2]], pch=19, cex=0.2, xlab="Intercept", ylab="Slope", bty="l")

# we can also plot the draws for all three parameters against each other
pairs(post, pch=19, cex=0.2)

# Figure 9.2(b) but also highlight the regression line based on the median intercept and slope
plot(dat$growth, dat$vote, type="n",
     xlab="Average recent growth in personal income",
     ylab="Incumbent party's vote share", bty="l")
apply(post[1:100,], 1, function(x) abline(a=x[1], b=x[2], col="gray40"))
abline(a=tab[1,1], b=tab[2,1], lwd=6)
points(dat$growth, dat$vote, pch=21, bg="black", col="white", lwd=3)

# we can also draw all 4000 lines, using alpha blending (i.e., making the
# lines semi-transparent) to indicate what lines are more or less plausible
plot(dat$growth, dat$vote, type="n",
     xlab="Average recent growth in personal income",
     ylab="Incumbent party's vote share", bty="l")
apply(post, 1, function(x) abline(a=x[1], b=x[2], col=rgb(0,0,0,.05)))
abline(a=tab[1,1], b=tab[2,1], lwd=6)
points(dat$growth, dat$vote, pch=21, bg="black", col="white", lwd=3)

## Using the matrix of posterior simulations to express uncertainty about a
## parameter estimate or function of parameter estimates

# for illustration purposes only, compute the ratio of intercept over slope
adivb <- post[[1]] / post[[2]]

# this yields the posterior distribution for this derived parameter, which we
# can again summarize via the median and mad
c(median = median(adivb), mad = mad(adivb))

# plot the kernel density estimate of the posterior for this derived parameter
plot(density(adivb))

# there are a few very extreme values, so zoom in on where most of the mass of
# the posterior distribution is
plot(density(adivb), xlim=c(0,50))

############################################################################

### 9.2: Prediction and uncertainty: predict, posterior_linpred, and posterior_predict

## Point prediction using predict

# note that coef() gives the median of the intercept and slope and we could
# use these values to make the point prediction
coef(res)
tab

# so we can make the point prediction manually as follows
coef(res)[[1]] + coef(res)[[2]] * 2.0

# but this is not exactly what predict() does
xnew <- data.frame(growth=2.0)
y_point_pred <- predict(res, newdata=xnew)
y_point_pred

# predict() actually computes the predicted values based on all regression
# lines and then takes the mean of those values
mean(post[[1]] + post[[2]] * 2.0)

## Linear predictor with uncertainty using posterior_linpred or posterior_epred

# posterior_linpred() does the same thing, but gives the individual predictions
y_linpred <- posterior_linpred(res, newdata=xnew)
head(y_linpred)

# taking their mean again gives the same value
mean(y_linpred[,1])

# but now we can also visualize the posterior for the predicted value
plot(density(y_linpred[,1]))

# and compute other summary statistics
median(y_linpred[,1])
sd(y_linpred[,1])
mad(y_linpred[,1])

## Predictive distribution for a new observation using posterior_predict

# note that above, we are making a prediction for the mean of y when x = 2.0,
# but now we want to make a prediction for an individual election; we can do
# so with the posterior_predict() function
y_pred <- posterior_predict(res, newdata=xnew)
head(y_pred)

# so this yields the posterior distribution for the predicted value of y for
# an individual election, which we can again summarize in various ways
mean(y_pred[,1])
sd(y_pred[,1])
median(y_pred[,1])
mad(y_pred[,1])

# while the mean (or median) values are essentially the same for the predicted
# mean and the predicted value of an individual election, the uncertainty in
# predicting an individual election is much larger

# but now we can also visualize the posterior for the predicted value
plot(density(y_linpred[,1]))

