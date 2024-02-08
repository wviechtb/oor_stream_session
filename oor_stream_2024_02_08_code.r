############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-02-08
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 11.6 - ?
#
# last updated: 2024-02-08

############################################################################

## 11.6 Generalized linear models

# copy the mtcars dataset to dat and inspect the dataset

dat <- mtcars
dat

# fit a linear regression model using lm()

res <- lm(mpg ~ hp + wt + am, data=dat)
summary(res)

# fit the same model using glm()

res <- glm(mpg ~ hp + wt + am, family=gaussian, data=dat)
summary(res)

# do a median split on the mpg variable (1 = high mpg, 0 = low mpg), so that
# we have a dichotomous outcome variable for doing logistic regression

dat$highmpg <- ifelse(dat$mpg > median(dat$mpg), 1, 0)
dat

res <- lm(highmpg ~ hp + wt + am, data=dat)
summary(res)

# fit a logistic regression model predicting highmpg from the same predictors
# as above

res <- glm(highmpg ~ hp + wt + am, family=binomial, data=dat)

# the dependent variable here is a 0/1 variable, which is assumed to have a
# binomial distribution, which, as a special case, is actually a Bernoulli
# distribution (https://en.wikipedia.org/wiki/Bernoulli_distribution); the
# mean of such a distribution is p, where p is the probability of seeing a 1;
# the model says that m^{-1}(p) = beta0 + beta1*x1 + beta2*x2 + ..., but what
# is this function m() and its inverse m^{-1}()? by default, it is the logit
# function, which is given by log(p/(1-p)), so the model says that the
# logit-transformed probability of a 1 is a linear function of one or multiple
# predictors


############################################################################

# - regression model with log transformed outcome
# - ZIP (zero-inflated Poisson model)

############################################################################
