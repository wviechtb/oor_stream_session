############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-04-17
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 13.1 - ?
#
# last updated: 2025-04-17

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### 13.1: Logistic regression with a single predictor

# make copies of the qlogis and plogis functions with easier to remember names
logit <- qlogis
invlogit <- plogis

# Figure 13.1(a): illustrate the inverse logit function
xs <- seq(-6, 6, length.out=1000)
ys <- invlogit(xs)
op <- par(mar=c(5,5,4,2), xaxs="i", yaxs="i", las=1)
plot(xs, ys, type="l", bty="l", xlab="x", ylab=expression("logit"^-1 * (x)), ylim=c(0,1))
title(expression(y == logit^-1 * (x)))
segments(0, 0, 0, invlogit(0), lty="dotted")
segments(-6, invlogit(0), 0, invlogit(0), lty="dotted")
par(op)

## Example: modeling political preference given income

# download the dataset if it doesn't already exist
if (!file.exists("nes.txt")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/NES/data/nes.txt", destfile="nes.txt")

# read in the dataset
dat <- read.table("nes.txt", header=TRUE)

# inspect the first six rows of the dataset
head(dat)

# keep only the data from 1992 and exclude respondents who preferred other candidates or had no opinion
ok <- dat$year==1992 & !is.na(dat$rvote) & !is.na(dat$dvote) & (dat$rvote==1 | dat$dvote==1)
dat <- dat[ok,]

# Figure 13.2 (left): show the fitted regression line
op <- par(mgp=c(3,0.5,0), las=1)
plot(jitter(rvote, amount=0.03) ~ jitter(income, amount=0.1), data=dat, bty="l",
     pch=19, cex=0.2, xlim=c(-1,7), xaxt="n", xlab="Income", ylab="Pr(Republican vote)")
axis(side=1, at=1:5, labels=c("1\n(poor)", 2:4, "5\n(rich)"), padj=1)

# fit the logistic regression model predicting rvote from income
res <- stan_glm(rvote ~ income, family=binomial(link="logit"), data=dat, refresh=0)
print(res, digits=2)

# add the regression line giving the probability of voting Republican to the plot
curve(invlogit(coef(res)[1] + coef(res)[2]*x), add=TRUE)
curve(invlogit(coef(res)[1] + coef(res)[2]*x), add=TRUE, from=1, to=5, lwd=6)

## The logistic regression model

# sidenote: in principle, we could use a standard regression model to model
# the relationship between the 0/1 outcome variable and the predictor (this is
# called a 'linear probability model'; see Wikipedia for further details:
# https://en.wikipedia.org/wiki/Linear_probability_model); this can often give
# very similar results, at least for parts of the line from the logistic
# regression model
res.lm <- stan_glm(rvote ~ income, data=dat, refresh=0)
curve(coef(res.lm)[1] + coef(res.lm)[2]*x, add=TRUE, col="red")

# predicted probability of voting Republican for income = 1, 2, ..., 5
pred <- invlogit(coef(res)[1] + coef(res)[2]*1:5)
round(pred, digits=2)

# difference in probabilities for income = 2 versus income = 1
pred[2] - pred[1]

# difference in probabilities for income = 3 versus income = 2 and so on ...
pred[3] - pred[2]
pred[4] - pred[3]
pred[5] - pred[4]

# note that these are not the same even though the difference in the income
# variable is always one point; this is just how things work for logistic
# regression models

## Fitting the model using stan_glm and displaying uncertainty in the fitted model

# Figure 13.2 (right)
plot(jitter(rvote, amount=0.03) ~ jitter(income, amount=0.1), data=dat, bty="l",
     pch=19, cex=0.2, xlim=c(-1,7), xaxt="n", xlab="Income", ylab="Pr(Republican vote)")
axis(side=1, at=1:5, labels=c("1\n(poor)", 2:4, "5\n(rich)"), padj=1)

# extract samples from the posterior distributions of the intercept and slope parameters
post <- as.data.frame(res)

# add 20 regression lines based on these samples
invisible(apply(post[1:20,], 1, function(b) curve(invlogit(b[1] + b[2]*x), add=TRUE, col="gray80")))

# add the regression line based on the median values of the sampled coefficients
curve(invlogit(coef(res)[1] + coef(res)[2]*x), add=TRUE, lwd=2)

# recall: coef() extracts the median values of the posterior distributions
coef(res)
apply(post, 2, median)

# reset the plot settings to the defaults
par(op)

############################################################################

### 13.2 Interpreting logistic regression coefficients and the divide-by-4 rule

## Evaluation at and near the mean of the data

# predicted probability of voting Republican when x is equal to the mean
round(invlogit(coef(res)[[1]] + coef(res)[[2]]*mean(dat$income)), digits=2)

# difference in probability when income = 3 versus when income = 2
round(pred[3] - pred[2], digits=2)

## The divide-by-4 rule

# compute the predicted probability of voting Republican for income from 1 to
# 5 and from 2 to 6 for 1000 values
xs1 <- seq(1, 5, length.out=10000)
xs2 <- seq(2, 6, length.out=10000)
preds1 <- invlogit(coef(res)[1] + coef(res)[2]*xs1)
preds2 <- invlogit(coef(res)[1] + coef(res)[2]*xs2)

# plot the difference in probabilities as a function of the income
plot(xs2, preds2 - preds1, type="l", bty="l", ylab="Difference in probabilities")

# horizontal dotted line at the largest difference
abline(h=max(preds2 - preds1), lty="dotted")

# we see that the largest difference is =~ slope / 4 for the predictor
max(preds2 - preds1)
coef(res)[[2]] / 4

# we can also figure out where this largest difference occurs
xs1[which.max(preds2 - preds1)]
xs2[which.max(preds2 - preds1)]

# so when income is equal to equal to 4.8 versus 3.8; so for income equal to 5
# versus 4 we are actually quite close to that upper bound
pred[5] - pred[4]

## Interpretation of coefficients as odds ratios

# predicted probability of voting Republican for income = 1, 2, ..., 5
pred <- invlogit(coef(res)[1] + coef(res)[2]*1:5)
round(pred, digits=2)

# we saw earlier that difference in probabilities are not constant; but maybe
# ratios of probabilities, so let's try that
pred[2] / pred[1]
pred[3] / pred[2]
pred[4] / pred[3]
pred[5] / pred[4]

# nope, not constant either ...

# compute the corresponding odds of voting Republican
odds <- pred / (1 - pred)
round(odds, digits=2)

# but it turns out that ratios of odds are constant
odds[2] / odds[1]
odds[3] / odds[2]
odds[4] / odds[3]
odds[5] / odds[4]

# the logistic regression model gives: log(p/(1-p)) given x = alpha + beta * x
#
# let's compute the difference in log odds when x is one unit higher versus
# not, so the difference between:
#
# log(p/(1-p)) given x+1 = alpha + beta * (x+1)
#
# versus
#
# log(p/(1-p)) given x   = alpha + beta * x
#
# so
#
# log(p/(1-p)) given x+1 - log(p/(1-p)) given x
#
# is equal to
#
# (alpha + beta * (x+1)) - (alpha + beta * x) = beta
#
# and log(p/(1-p)) given x+1 - log(p/(1-p)) given x
#
# is equal to
#
# log(p/(1-p)) given x+1 / p/(1-p)) given x)
#
# which is really log(odds given x+1 / odds given x)
#
# which is log(odds ratio) when x differs by one unit, which is equal to beta,
# so exp(beta) = odds ratio

# in our analysis above, the odds ratio is therefore
exp(coef(res)[[2]])

# so the odds of voting Republican are 1.39 times higher when comparing
# individuals that differ by one unit on income

## Coefficient estimates and standard errors

# while our best guess (i.e., the median of the posterior distribution) about
# the size of the slope is around .33, there is uncertainty as to how large
# the slope actually is
round(coef(res)[[2]], digits=2)

# we are around 95% certain that the slope is between these bounds (assuming
# normality of the posterior distribution for the slope)
round(coef(res)[[2]] - 2 * se(res)[[2]], digits=2)
round(coef(res)[[2]] + 2 * se(res)[[2]], digits=2)

# examine the posterior distribution for the slope
hist(post$income, breaks=50, xlab="Slope for income", main="")

# or we could just construct a 95% percentile interval based on the sampled values
round(quantile(post$income, prob=c(.025, .975)), digits=2)

## Statistical significance

# check how many standard errors the (median) slope is away from 0
coef(res)[[2]] / se(res)[[2]]

## Displaying the results of several logistic regressions

# read in the dataset again
dat <- read.table("nes.txt", header=TRUE)

# exclude respondents who preferred other candidates or had no opinion
ok <- !is.na(dat$rvote) & !is.na(dat$dvote) & (dat$rvote==1 | dat$dvote==1)
dat <- dat[ok,]

# find the unique years
years <- unique(dat$year)
years

# fit the model for the data in each unique year
res <- lapply(years, function(y) stan_glm(rvote ~ income, family=binomial(link="logit"), data=dat, refresh=0, subset=year==y))

# extract the slopes and corresponding standard errors from the models
b <- sapply(res, coef)[2,]
se <- sapply(res, se)[2,]

# Figure 13.4: Plot of the slope coefficients over the years
plot(years, b, pch=19, xlab="Year", ylab="Coefficient of income", bty="l",
     xlim=c(1950,2000), ylim=c(-0.05,0.5), las=1)
segments(years, b-se, years, b+se)
abline(h=0, lty="dotted")

############################################################################

### 13.3 Predictions and comparisons

# read in the dataset again
dat <- read.table("nes.txt", header=TRUE)

# keep only the data from 1992 and exclude respondents who preferred other candidates or had no opinion
ok <- dat$year==1992 & !is.na(dat$rvote) & !is.na(dat$dvote) & (dat$rvote==1 | dat$dvote==1)
dat <- dat[ok,]

# fit the logistic regression model predicting rvote from income
res <- stan_glm(rvote ~ income, family=binomial(link="logit"), data=dat, refresh=0)
print(res, digits=2)

# extract samples from the posterior distributions of the intercept and slope parameters
post <- as.data.frame(res)

## Point prediction using predict

# predicted probability when income = 5
newdat <- data.frame(income=5)
pred <- predict(res, type="response", newdata=newdat)
pred

# so we see that predict() gives us the mean of the predicted probabilities
# computed from the sampled values from the posterior distributions
mean(apply(post, 1, function(b) invlogit(b[[1]] + b[[2]] * 5)))

# compute the mean of the posterior samples
b <- apply(post, 2, mean)
invlogit(b[[1]] + b[[2]] * 5)

## Linear predictor with uncertainty using posterior_linpred

# obtain the predicted values when income = 5 using posterior_linpred()
linpred <- posterior_linpred(res, newdata=newdat)
head(linpred)

# this is the same as computing the predicted values based on the posterior
# samples of the intercept and slope as we did above
head(cbind(linpred, apply(post, 1, function(b) b[[1]] + b[[2]] * 5)))

# note: here we are getting the predicted log odds, which is typically not
# what we are interested in

## Expected outcome with uncertainty using posterior_epred

# obtain the predicted probabilities
epred <- posterior_epred(res, newdata=newdat)
head(epred)

# again, we could do this manually based on the sampled intercept and slope values
head(cbind(epred, apply(post, 1, function(b) invlogit(b[[1]] + b[[2]] * 5))))

# mean and SD of the predicted probabilities
round(c(mean=mean(epred), sd=sd(epred)), digits=2)

## Predictive distribution for a new observation using posterior_predict

# simulate the votes of individual voters with income = 5
postpred <- posterior_predict(res, newdata=newdat)
head(postpred)

# we can think of the mean (i.e., proportion of votes for a Republican) as the
# point prediction for the probability of voting Republican
mean(postpred)

## Prediction given a range of input values

# do the same as above, but for income = 1, ..., 5
newdat <- data.frame(income=1:5)
pred     <- predict(res, type="response", newdata=newdat)
linpred  <- posterior_linpred(res, newdata=newdat)
epred    <- posterior_epred(res, newdata=newdat)
postpred <- posterior_predict(res, newdata=newdat)
