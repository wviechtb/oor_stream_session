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

res <- lm(mpg ~ hp + vs, data=dat)
summary(res)

# fit the same model using glm()

res <- glm(mpg ~ hp + vs, family=gaussian, data=dat)
summary(res)

# do a median split on the mpg variable (1 = high mpg, 0 = low mpg), so that
# we have a dichotomous outcome variable for doing logistic regression

dat$highmpg <- ifelse(dat$mpg > median(dat$mpg), 1, 0)
dat

res <- lm(highmpg ~ hp + vs, data=dat)
summary(res)

# fit a logistic regression model predicting highmpg (but see below) from the
# same predictors as above

res <- glm(highmpg ~ hp + vs, family=binomial, data=dat)
summary(res)

# the dependent variable here is a 0/1 variable, which is assumed to have a
# binomial distribution, which, as a special case, is actually a Bernoulli
# distribution (https://en.wikipedia.org/wiki/Bernoulli_distribution); the
# mean of such a distribution is p, where p is the probability of seeing a 1;
# the model says that m^{-1}(p) = beta0 + beta1*x1 + beta2*x2 + ..., but what
# is this function m() and its inverse m^{-1}()? by default, m^{-1}() is the
# logit function, which is given by log(p/(1-p)), so the model says that the
# logit-transformed probability of a 1 is a linear function of one or multiple
# predictors (sidenote: p/(1-p) is the so-called 'odds' of seeing a 1 and
# hence log(p/(1-p)) are the so-called 'log odds'); so here, the default link
# function is the logit transformation

# to illustrate, say p=0.7, then the logit-transformed value is as follows

log(0.7 / (1 - 0.7))

# which can also be computed with qlogis() (i.e., eta = m^{-1}() = qlogis())

qlogis(0.7)

# note that qlogis() maps probabilities between 0 and 1 to the real line
# (i.e., to minus to plus infinity); for example:

qlogis(0)
qlogis(0.00000000001)
qlogis(0.5)
qlogis(0.99999999999)
qlogis(1)

# so based on the the model, we can get the predicted log odds (of high mpg)

predict(res, newdata=data.frame(hp=100, vs=1))

# in the notation explained in this section, this value is eta (or more
# precisely, eta with a hat on top of it, since it is a predicted value); if
# we want the (estimated/predicted) probability of high mpg, we need to
# back-transform this, so we need to apply m() to this value, which we can do
# using the plogis() function (so then we are getting p = m(eta))

plogis(predict(res, newdata=data.frame(hp=100, vs=1)))

# note: plogis() maps values between minus and plus infinity to 0 and 1)

plogis(-Inf)
plogis(-3)
plogis(0)
plogis(3)
plogis(Inf)

# we can do this directly with predict

predict(res, newdata=data.frame(hp=100, vs=1), type="response")

# so, by using the logit link, we are guaranteed that the predicted
# probability is always a value between 0 and 1 (which is good, since that is
# the range for probabilities)

# so how can we interpret the estimated model coefficients?

# let's start with the intercept

coef(res)[[1]]

# the intercept is the estimated log odds of high mpg when hp=0 and when vs=0,
# which we can turn into the predicted probability again with plogis()

plogis(coef(res)[[1]])

# but of course a car with hp=0 doesn't exist, so this is extrapolation beyond
# the range of our data (one could center 'hp' at some more meaningful value,
# so that the intercept is also more sensible)

# now let's look at the coefficient for hp

coef(res)[[2]]

# this estimates how the log odds of high mpg changes for a one-unit increase
# in hp (i.e., what is the difference in log odds when hp = x + 1 versus when
# hp = x); for example, say we compare two cars where one has hp=101 and the
# other has hp=100, then the predicted log odds are as follows

coef(res)[[1]] + coef(res)[[2]] * 101
coef(res)[[1]] + coef(res)[[2]] * 100

# and the difference between those two is the coefficient for hp

(coef(res)[[1]] + coef(res)[[2]] * 101) - (coef(res)[[1]] + coef(res)[[2]] * 100)
(coef(res)[[2]] * 101) - (coef(res)[[2]] * 100)
coef(res)[[2]] * (101 - 100)
coef(res)[[2]] * 1
coef(res)[[2]]

# of course cars can differ not just by 1 hp, but say by 10 hp, so then we
# just take 10 times the slope to figure what the difference is

coef(res)[[2]] * 10

# what does this imply about the difference in probabilities? the predicted
# probabilities of high mpg when hp=110 versus hp=100 (when vs=0) are

plogis(coef(res)[[1]] + coef(res)[[2]] * 110)
plogis(coef(res)[[1]] + coef(res)[[2]] * 100)

# and the difference between these two probabilities is

plogis(coef(res)[[1]] + coef(res)[[2]] * 110) - plogis(coef(res)[[1]] + coef(res)[[2]] * 100)


coef(res)[[1]] + coef(res)[[2]] * 101



# we could do the same thing for the coefficient for 'vs',

############################################################################

# - regression model with log transformed outcome
# - ZIP (zero-inflated Poisson model)

############################################################################
