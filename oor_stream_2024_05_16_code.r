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
x_new <- data.frame(growth=2.0)
y_point_pred <- predict(res, newdata=x_new)
y_point_pred

# predict() actually computes the predicted values based on all regression
# lines and then takes the mean of those values
mean(post[[1]] + post[[2]] * 2.0)

## Linear predictor with uncertainty using posterior_linpred or posterior_epred

# posterior_linpred() does the same thing, but gives the individual predictions
y_linpred <- posterior_linpred(res, newdata=x_new)
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
y_pred <- posterior_predict(res, newdata=x_new)
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

# visualize the posterior for the predicted value of an individual election
# and also add the posterior for the mean
plot(density(y_pred[,1]), col="dodgerblue", lwd=3, ylim=c(0,.40), main="", bty="l")
lines(density(y_linpred[,1]), col="firebrick", lwd=3)
legend("topright", inset=.02, lwd=3, col=c("dodgerblue", "firebrick"),
       legend=c("Posterior for E(y|x=2.0)", "Posterior for y|x=2.0"))

# do the prediction of y|x=2.0 manually
y_pred_manual <- post[[1]] + post[[2]] * 2.0 + rnorm(nrow(post), mean=0, sd=post[[3]])
head(y_pred_manual)

# also add the posterior based on our manual calculations
lines(density(y_pred_manual), col="forestgreen", lwd=3)

# we see that this is essentially identical to what posterior_predict() is
# doing, but because the additional uncertainly in predicting y is random,
# there are minor discrepancies

# we can derive further quantities from the posterior of the predicted value
# of y when x=2.0; for example, the posterior distribution for y being greater
# than 50% (which is of course a dichotomous variable)
table(y_pred[,1] > 50)

# and this distribution we can again summarize
mean(y_pred[,1] > 50)

# so the chances of winning the election is around 71% when x=2.0; this was in
# fact the economic growth value leading up to the 2016 presidential election
# when Hillary Clinton (as the incumbent candidate) ran against Donald Trump;
# according to the model, she had high chances of winning, but we all know how
# that one turned out

## Prediction given a range of input values

# predictions for x = -2.0, -1.5, ..., 4.0
new_grid <- data.frame(growth=seq(-2.0, 4.0, 0.5))
y_point_pred_grid <- predict(res, newdata=new_grid)
y_linpred_grid    <- posterior_linpred(res, newdata=new_grid)
y_pred_grid       <- posterior_predict(res, newdata=new_grid)

# examine the predicted values
y_point_pred_grid
head(y_linpred_grid)
head(y_pred_grid)

## Propagating uncertainty

# simulate the uncertainty in economic growth values (note: we use much more
# uncertainty here compared to the value in the book)
x_new <- rnorm(nrow(post), mean=2.0, sd=0.9)

# and make use of these in making a prediction of y
y_pred <- post[[1]] + post[[2]] * x_new + rnorm(nrow(post), mean=0, sd=post[[3]])

# show that this adds additional uncertainty to the posterior distribution
lines(density(y_pred), col="orange", lwd=3)

# note: we really have to bump of the SD for x_new to actually see this
# increase in uncertainty (with an SD of 0.3 as in the book, the difference is
# not really noticeable)

## Simulating uncertainty for the linear predictor and new observations

# instead of using the earnings dataset, we stick to the same dataset above
# and illustrate the part that is new, namely equations 9.1 and 9.2, which are
# used when fitting a model with lm()
res2 <- lm(vote ~ growth, data=dat)
summary(res2)

# we can make the two different types of predictions using predict()
x_new <- data.frame(growth=2.0)
predict(res2, newdata=x_new, interval="confidence") # this uses (9.1)
predict(res2, newdata=x_new, interval="prediction") # this uses (9.2)

############################################################################

### 9.3: Prior information and Bayesian synthesis

## Expressing data and prior information on the same scale

# note: we already touched on the concepts of this section in section 1.5
# (which we discussed during the stream on 2023-03-23)

## Bayesian information aggregation

theta_hat_prior <- 0.524
se_prior <- 0.041
n <- 400
y <- 190
theta_hat_data <- y/n
se_data <- sqrt(theta_hat_data*(1-theta_hat_data)/n)
theta_hat_bayes <- (theta_hat_prior/se_prior^2 + theta_hat_data/se_data^2) / (1/se_prior^2 + 1/se_data^2)
se_bayes <- sqrt(1/(1/se_prior^2 + 1/se_data^2))
theta_hat_bayes
se_bayes



############################################################################
