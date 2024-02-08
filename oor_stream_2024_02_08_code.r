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

# note: wt is given per 1000 pounds and am=1 for manual transmission and am=0
# for automatic transmission

# fit a linear regression model using lm()

res <- lm(mpg ~ wt + am, data=dat)
summary(res)

# fit the same model using glm()

res <- glm(mpg ~ wt + am, family=gaussian, data=dat)
summary(res)

# do a split on the mpg variable (1 = high (>22) mpg, 0 = low (<= 22) mpg), so
# that we have a dichotomous outcome variable for doing logistic regression
# (note: 22 miles per gallon is of course horrible gas mileage by today's
# standards, but these are old cars, so ...)

dat$highmpg <- ifelse(dat$mpg > 22, 1, 0)
dat

# fit a logistic regression model predicting high mpg (but see below) from the
# same predictors as above

res <- glm(highmpg ~ wt + am, family=binomial, data=dat)
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

# we can see what the default link is for a particular 'family' under the
# following help file

help(family)

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

# so based on the model, we can get the predicted log odds (of high mpg) when
# wt=2 (so 2000 pounds) and am=1 (manual transmission) as follows

predict(res, newdata=data.frame(wt=2, am=1))

# in the notation explained in this section, this value is eta (or more
# precisely, eta with a hat on top of it, since it is a predicted value); if
# we want the (estimated/predicted) probability of high mpg, we need to
# back-transform this, so we need to apply m() to this value, which we can do
# using the plogis() function (so then we are getting p = m(eta))

plogis(predict(res, newdata=data.frame(wt=2, am=1)))

# note: plogis() maps values between minus and plus infinity to 0 and 1)

plogis(-Inf)
plogis(-3)
plogis(0)
plogis(3)
plogis(Inf)

# we can do this directly with predict

predict(res, newdata=data.frame(wt=2, am=1), type="response")

# so, by using the logit link, we are guaranteed that the predicted
# probability is always a value between 0 and 1 (which is good, since that is
# the range for probabilities)

# so how can we interpret the estimated model coefficients?

# let's start with the intercept

coef(res)[[1]]

# the intercept is the estimated log odds of high mpg when wt=0 and when am=0,
# which we can turn into the predicted probability again with plogis()

plogis(coef(res)[[1]])

# but of course a car with wt=0 doesn't exist, so this is extrapolation beyond
# the range of our data (one could center 'wt' at some more meaningful value,
# so that the intercept is also more sensible)

# now let's look at the coefficient for wt

coef(res)[[2]]

# this estimates how the log odds of high mpg changes for a one-unit increase
# in wt (i.e., what is the difference in log odds when wt = x + 1 versus when
# wt = x); for example, say we compare two cars where one has wt=3 (3000
# pounds) and the other has wt=2 (2000 pounds), then the predicted log odds
# are as follows

coef(res)[[1]] + coef(res)[[2]] * 3
coef(res)[[1]] + coef(res)[[2]] * 2

# and the difference between those two is the coefficient for wt

(coef(res)[[1]] + coef(res)[[2]] * 3) - (coef(res)[[1]] + coef(res)[[2]] * 2)
(coef(res)[[2]] * 3) - (coef(res)[[2]] * 2)
coef(res)[[2]] * (3 - 2)
coef(res)[[2]] * 1
coef(res)[[2]]

# note that it does not matter what the wt values above are, as long as the
# difference between them is one unit, we will always get coef(res)[[2]]

# of course cars can differ not just by 1 wt, but say by 3 wt, so then we
# just take 3 times the slope to figure what the difference is

coef(res)[[2]] * 3

# again, it does not matter if we are talking 4 versus 1 or 5 versus 2, this
# is always the difference in the log odds for a difference in 3 wt

# what does this imply about the difference in probabilities? the predicted
# probabilities of high mpg when wt=3 versus wt=2 (when am=0) are

plogis(coef(res)[[1]] + coef(res)[[2]] * 3)
plogis(coef(res)[[1]] + coef(res)[[2]] * 2)

# and the difference between these two probabilities is

plogis(coef(res)[[1]] + coef(res)[[2]] * 3) - plogis(coef(res)[[1]] + coef(res)[[2]] * 2)

# however, now it *does* matter what the absolute wt values are

plogis(coef(res)[[1]] + coef(res)[[2]] * 4) - plogis(coef(res)[[1]] + coef(res)[[2]] * 3)

# even though the difference in wt is 1 in both of these two examples

# what is typically reported in logistic regression is not the difference in
# probabilities, but the ratio of the odds (i.e., the odds ratio)

# consider a car with wt=3, then the odds of high mpg is as follows

p3 <- plogis(coef(res)[[1]] + coef(res)[[2]] * 3)
p3 / (1 - p3)

# and the odds of high mpg for a car wt=2 is as follows

p2 <- plogis(coef(res)[[1]] + coef(res)[[2]] * 2)
p2 / (1 - p2)

# the ratio of these two odds is the odds ratio

(p3 / (1 - p3)) / (p2 / (1 - p2))

# it turns out that we can get this odds ratio is we exponentiate the
# coefficient for wt

exp(coef(res)[[2]])

# for such an odds ratio, the absolute wt values do not matter

p4 <- plogis(coef(res)[[1]] + coef(res)[[2]] * 4)
p3 <- plogis(coef(res)[[1]] + coef(res)[[2]] * 3)
(p4 / (1 - p4)) / (p3 / (1 - p3))

# we could do the same thing for the coefficient for 'am'

exp(coef(res)[[3]])

# so the odds of high mpg for cars with a manual transmission (am=1) is ~ 1/10
# of the odds of high mpg for cars with an automatic transmission (am=0)
# (leaving aside that the difference is not actually significant)

# we will skip the example given in this section (the "small, artificial
# example, from Silvey")

# instead, let's do a different type of GLM, namely a Poisson regression
# model; this is often used when we have an outcome variable that is a count
# of something; then we might assume that the variable follows a Poisson
# distribution (https://en.wikipedia.org/wiki/Poisson_distribution)

# consider the following dataset

dat <- InsectSprays
dat

# the count variable indicates the number of inspects on 'agricultural
# experimental units' treated with different types of insecticides (spray)

# fit a Poisson regression model using spray as a categorical predictor

res <- glm(count ~ spray, family=poisson, data=dat)
summary(res)

# the default link for family=poisson is the log link; so in this case, we are
# modeling the mean of the Poisson distribution (which is often denoted as
# lambda) as log(lambda) = beta0 + beta1*x1 + beta2*x2 + ...

# the estimated log-transformed mean for spray type A is just the intercept

coef(res)[[1]]

# hence, the estimated mean count for spray type A is

exp(coef(res)[[1]])

# the estimated log-transformed mean for spray type B is the intercept plus
# the coefficient for sprayB

coef(res)[[1]] + coef(res)[[2]]

# hence, the estimated mean count for spray type B is

exp(coef(res)[[1]] + coef(res)[[2]])

# we can again use the predict function to compute these predicted mean counts
# (for all spray types) as follows

predict(res, newdata=data.frame(spray=c("A","B","C","D","E","F")), type="response")

# fit the reduced model that assumes that the mean count does not depend on
# the spray type and then compare this model against the one above

res0 <- glm(count ~ 1, family=poisson, data=dat)
anova(res0, res, test="Chisq")

# note that the predicted mean counts above are the same as the mean counts
# for the different spray types as observed in our data

by(dat$count, dat$spray, mean)

# in a Poisson distribution, the mean is equal to the variance (see the
# Wikipedia link above); but if we compute the variances of the counts for the
# different spray types, we see that the variances are consistently above the
# means (except for type E); this is called 'overdispersion'

tab <- data.frame(mean     = by(dat$count, dat$spray, mean),
                  variance = by(dat$count, dat$spray, var))
tab$ratio <- tab$variance / tab$mean
tab

# this violates an assumption of the Poisson distribution; we can relax this
# assumption with the quasi-Poisson family, which allows the variance of the
# counts to differ from the means by a multiplicative factor

res2 <- glm(count ~ spray, family=quasipoisson, data=dat)
summary(res2)

# in this simple case, this factor (1.507713) is just the average of the
# ratios we saw above

mean(tab$ratio)

# note that the coefficients have not changed, since we still want to get the
# same predicted mean counts (which match the observed means); however, the
# standard errors are now larger, to reflect the increased uncertainty in the
# estimates due to the larger variances; in fact, the standard errors are
# increased by a factor that is equal to the square root of the overdispersion
# parameter

coef(summary(res2))[,"Std. Error"] / coef(summary(res))[,"Std. Error"]
sqrt(mean(tab$ratio))

############################################################################

dat <- data.frame(x = c(20,35,45,55,70), n = rep(50,5), y = c(6,17,26,37,44))
dat

xs <- 20:70

res1 <- glm(cbind(y,n-y) ~ x, family=binomial, data=dat)
summary(res1)
pred1 <- predict(res1, newdata=data.frame(x=xs), type="response")
pred1

res2 <- glm(y ~ x, family=poisson, data=dat)
summary(res2)
pred2 <- predict(res2, newdata=data.frame(x=xs), type="response")

plot(xs, pred1*50, type="l", lwd=2, ylim=c(0,50))
lines(xs, pred2, type="l", lwd=2, col="red")
points(dat$x, dat$y, pch=19)

res3 <- glm(y ~ x, family=poisson(link=sqrt), data=dat)
summary(res3)
pred3 <- predict(res3, newdata=data.frame(x=xs), type="response")
lines(xs, pred3, type="l", lwd=2, col="blue")

############################################################################

# relative risk regression
res1 <- glm(cbind(y,n-y) ~ x, family=binomial(link=log), data=dat)
summary(res1)

# estimated risk ratio
exp(coef(res1)[[2]])

predict(res1, newdata=data.frame(x=21:70), type="response") / predict(res1, newdata=data.frame(x=20:69), type="response")

res2 <- glm(cbind(y,n-y) ~ x, family=binomial, data=dat)
predict(res2, newdata=data.frame(x=21:70), type="response") / predict(res2, newdata=data.frame(x=20:69), type="response")

log(p|x+1) - log(p|x) = log(p1) - log(p0) = log(p1/p0)

res4 <- glm(cbind(y,n-y) ~ x, family=binomial(link="identity"), data=dat)
summary(res4)

(p|x+1) - (p|x) = p1 - p0


res3 <- glm(y ~ x + offset(log(n)), family=poisson, data=dat)
summary(res3)

# log(mean) = beta0 + beta1*x + log(n)
# log(mean) - log(n) = beta0 + beta1*x
# log(mean/n) = beta0 + beta1*x

############################################################################

# - regression model with log transformed outcome
# - ZIP (zero-inflated Poisson model)

############################################################################
