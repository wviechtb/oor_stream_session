############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-02-27
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 5.1 - ?
#
# last updated: 2025-02-27

############################################################################

### 5.1: Spurious association

# load the rethinking package
library(rethinking)

# get the WaffleDivorce dataset and put it into 'dat'
dat <- get(data(WaffleDivorce))

# Figure 5.2
par(mfrow=c(1,2))
plot(Divorce ~ Marriage, data=dat, pch=21, bg="gray", bty="l",
     xlab="Marriage rate", ylab="Divorce rate")
plot(Divorce ~ MedianAgeMarriage, data=dat, pch=21, bg="gray", bty="l",
     xlab="Median age marriage", ylab="Divorce rate")
par(mfrow=c(1,1))

# standardize some variables
dat$D <- c(scale(dat$Divorce))
dat$M <- c(scale(dat$Marriage))
dat$A <- c(scale(dat$MedianAgeMarriage))

# SD of the MedianAgeMarriage variable
sd(dat$MedianAgeMarriage)

# define the regression model (predicting the standardized divorce rate D from
# the standardized median age at marriage A)
model1 <- alist(D ~ dnorm(mu, sigma),
                mu <- a + bA * A,
                a ~ dnorm(0, 0.2),
                bA ~ dnorm(0, 0.5),
                sigma ~ dexp(1))

# fit the model using the quadratic approximation approach
res1 <- quap(model1, data=dat)
precis(res1, prob=0.95)

# sample 1000 values from the prior distributions
set.seed(10)
prior <- as.data.frame(extract.prior(res1))
head(prior)

# plot 50 of the regression lines based on the sampled values
plot(NA, xlim=c(-2,2), ylim=c(-2,2), xlab="Median age marriage (std)",
     ylab="Divorce rate (std)", bty="l")
apply(prior[1:50,], 1, function(par) abline(par[1], par[2], lwd=1.2, col="gray30"))

# extract 1000 samples from the posterior distributions of the intercept,
# slope, and error standard deviation
set.seed(10)
post <- extract.samples(res1, n=1000)

# compute the predicted value (i.e., the expected value of D) based on each of
# the sampled intercept and slope values obtained above from A is equal to -3
pred <- apply(post, 1, function(par) par[1] + par[2] * -3)
head(pred)

# using the link() function, we can do the same thing; in fact, we will do the
# same when A is equal to -3, -2.8, ..., 3.2 (note: we keep resetting the seed
# to obtain the exact same values above and from link())
set.seed(10)
A_seq <- seq(from=-3, to=3.2, by=0.2)
mu <- link(res1, data=list(A=A_seq))

# double-check that the predicted values for A = -3 are the same as the ones
# we obtained above manually
head(mu[,1])

# compute percentile interval of mean
mu.mean <- apply(mu, 2, mean)
mu.pi   <- apply(mu, 2, PI, prob=0.95)

# plot it all
plot(D ~ A, data=dat, pch=21, bg="gray", xlab="Median age marriage (std)",
     ylab="Divorce rate (std)", bty="l")
lines(A_seq, mu.mean, lwd=2)
shade(mu.pi, A_seq)

# show the x- and y-values on the original scale
plot(D ~ A, data=dat, pch=21, bg="gray", xlab="Median age marriage (std)",
     ylab="Divorce rate (std)", bty="l", xaxt="n", yaxt="n")
axis(side=1, at=((23:30) - mean(dat$MedianAgeMarriage)) / sd(dat$MedianAgeMarriage), labels=23:30)
axis(side=2, at=((6:14) - mean(dat$Divorce)) / sd(dat$Divorce), labels=6:14)
lines(A_seq, mu.mean, lwd=2)
shade(mu.pi, A_seq)

# define the regression model (predicting the standardized divorce rate D from
# the standardized marriage rate M)
model2 <- alist(D ~ dnorm(mu, sigma),
                mu <- a + bM * M,
                a ~ dnorm(0, 0.2),
                bM ~ dnorm(0, 0.5),
                sigma ~ dexp(1))

# fit the model using the quadratic approximation approach
res2 <- quap(model2, data=dat)
precis(res2, prob=0.95)

## 5.1.1: Think before you regress

# install the dagitty package (if necessary)
#install.packages("dagitty")

# load the dagitty package
library(dagitty)

# draw the DAG described in the book
dag1 <- dagitty("dag{A -> D; A -> M; M -> D}")
coordinates(dag1) <- list(x=c(A=0, D=1, M=2), y=c(A=0, D=1, M=0))
drawdag(dag1)
graphics.off()

## 5.1.2: Testable implications

# check that all three variables are in fact not independent
cor(dat[c("D","A","M")])

# sidenote: to use correlations in this way to check that variables are in
# fact not independent, strictly speaking we would have to assume that the
# three variables jointly follow a multivariate normal distribution

# define the DAG where M does not have a direct influence on D
dag2 <- dagitty("dag{D <- A -> M}")
impliedConditionalIndependencies(dag2)

# check if there are conditional independencies in the first DAG
impliedConditionalIndependencies(dag1)

## 5.1.4: Approximating the posterior

# define the regression model with both M and A as predictors of D
model3 <- alist(D ~ dnorm(mu, sigma),
                mu <- a + bM * M + bA * A,
                a ~ dnorm(0, 0.2),
                bM ~ dnorm(0, 0.5),
                bA ~ dnorm(0, 0.5),
                sigma ~ dexp(1))

# fit the model using the quadratic approximation approach
res3 <- quap(model3, data=dat)
precis(res3, prob=0.95)

# plot the coefficients from all three models
plot(coeftab(res1, res2, res3), par=c("bA","bM"))

# Overthinking: Simulating the divorce example
N <- 50 # number of simulated States
age <- rnorm(N) # simulate A
mar <- rnorm(N, mean = 0 + -1*age) # simulate A -> M
div <- rnorm(N, mean = 0 +  1*age) # simulate A -> D

## 5.1.5: Plotting multivariate posteriors

# 5.1.5.1: Predictor residual plots

# define the regression model where we predict M from A
model4 <- alist(M ~ dnorm(mu, sigma),
                mu <- a + bAM * A,
                a ~ dnorm(0, 0.2),
                bAM ~ dnorm(0, 0.5),
                sigma ~ dexp(1))

# fit the model using the quadratic approximation approach
res4 <- quap(model4, data=dat)
precis(res4, prob=0.95)

# compute predicted values for M for each of the 50 states based on 1000
# sampled values from the posterior distributions of the intercept and slope
mu <- link(res4)
dim(mu)

# compute the mean of the predicted values for each state
mu_mean <- apply(mu, 2, mean)
mu_mean

# compute the residuals
mu_resid <- dat$M - mu_mean
mu_resid

# Figure 5.4 (upper left): plot of M (y-axis) on A (x-axis)
plot(M ~ A, data=dat, pch=21, bg="gray", bty="l",
     xlab="Age at marriage (std)", ylab="Marriage rate (std)")
abline(coef(res4)[1], coef(res4)[2], lwd=6)
segments(dat$A, mu_mean, dat$A, dat$M)
points(M ~ A, data=dat, pch=21, bg="gray")

# Figure 5.4 (lower left): plot of D (y-axis) on the residuals (x-axis)
plot(D ~ mu_resid, data=dat, pch=21, bg="gray", bty="l",
     xlab="Marriage rate residuals", ylab="Divorce rate (std)")

# define the regression model where we predict D from the residuals
model5 <- alist(D ~ dnorm(mu, sigma),
                mu <- a + bR * mu_resid,
                a ~ dnorm(0, 0.2),
                bR ~ dnorm(0, 0.5),
                sigma ~ dexp(1))

# fit the model using the quadratic approximation approach
res5 <- quap(model5, data=dat)

# add the regression line and PI bounds to the plot
resid_seq <- seq(from=-2, to=2, by=0.1)
mu <- link(res5, data=list(mu_resid=resid_seq))
mu.mean <- apply(mu, 2, mean)
mu.pi   <- apply(mu, 2, PI, prob=0.95)
lines(resid_seq, mu.mean, lwd=2)
shade(mu.pi, resid_seq)
abline(v=0, lty="dashed")

# now we do the same thing as above but predicting A from M
model6 <- alist(A ~ dnorm(mu, sigma),
                mu <- a + bAM * M,
                a ~ dnorm(0, 0.2),
                bAM ~ dnorm(0, 0.5),
                sigma ~ dexp(1))
res6 <- quap(model6, data=dat)
mu <- link(res6)
mu_mean <- apply(mu, 2, mean)
mu_resid <- dat$A - mu_mean

# Figure 5.4 (upper right): plot of A (y-axis) on M (x-axis)
plot(A ~ M, data=dat, pch=21, bg="gray", bty="l",
     xlab="Marriage rate (std)", ylab="Age at marriage (std)")
abline(coef(res6)[1], coef(res6)[2], lwd=6)
segments(dat$M, mu_mean, dat$M, dat$A)
points(A ~ M, data=dat, pch=21, bg="gray")

# Figure 5.4 (lower right): plot of D (y-axis) on the residuals (x-axis)
plot(D ~ mu_resid, data=dat, pch=21, bg="gray", bty="l",
     xlab="Age at Marriage residuals", ylab="Divorce rate (std)")

# fit the regression model where we predict D from the residuals
model7 <- alist(D ~ dnorm(mu, sigma),
                mu <- a + bR * mu_resid,
                a ~ dnorm(0, 0.2),
                bR ~ dnorm(0, 0.5),
                sigma ~ dexp(1))
res7 <- quap(model7, data=dat)

# add the regression line and PI bounds to the plot
resid_seq <- seq(from=-2, to=3, by=0.1)
mu <- link(res7, data=list(mu_resid=resid_seq))
mu.mean <- apply(mu, 2, mean)
mu.pi   <- apply(mu, 2, PI, prob=0.95)
lines(resid_seq, mu.mean, lwd=2)
shade(mu.pi, resid_seq)
abline(v=0, lty="dashed")

############################################################################

# digression: when going back to a more traditional frequentist framework, we
# can see the same idea at play and in fact get exact equivalence between the
# coefficients from the full regression model and the models where we use
# residuals to predict the outcome
res <- lm(Divorce ~ MedianAgeMarriage + Marriage, data=dat)
coef(res)
res <- lm(MedianAgeMarriage ~ Marriage, data=dat)
coef(lm(Divorce ~ resid(res), data=dat))
res <- lm(Marriage ~ MedianAgeMarriage, data=dat)
coef(lm(Divorce ~ resid(res), data=dat))

############################################################################

# 5.1.5.2: Posterior prediction plots

# call link() without specifying new data so it uses original data (so we get
# 1000 predicted values for each of the 50 states based on their A and M values)
mu <- link(res3)
dim(mu)

# summarize the predicted values for each state
mu_mean <- apply(mu, 2, mean)
mu_pi   <- apply(mu, 2, PI, prob=0.95)

# simulate observations # again no new data, so uses original data
D_sim <- sim(res3, n=10000)
D_PI  <- apply(D_sim, 2, PI, prob=0.95)

