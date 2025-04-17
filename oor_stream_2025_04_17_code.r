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