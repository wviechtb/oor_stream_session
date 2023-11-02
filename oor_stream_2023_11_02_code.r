############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-11-02
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 11.3 - ?
#
# last updated: 2023-11-02

############################################################################

## 11.3 Generic functions for extracting model information

# inspect the mtcars dataset again
mtcars

# fit a linear regression model predicting miles per gallon from horsepower
# and whether it is an automatic or manual transmission
res <- lm(mpg ~ hp + am, data=mtcars)

# print the 'res' object
res

# the above is the same as calling print() directly on the object
print(res)

# as we can see, just printing the 'res' object is rather uninformative; all
# this gives us is the model that was fitted and the model coefficients

# the lm() function returns an object of class 'lm'
class(res)

# if we just want to see what is inside of this object (which is really just a
# list), we can remove the class and just print the list contents
unclass(res)

# to get more information about the fitted model, use summary()
summary(res)

# we can extract the coefficients with coef()
coef(res)

# so we could manually compute the predicted mpg for cars with hp=100 and am=1
# (i.e., manual transmission)
b <- coef(res)
b[[1]] + b[[2]]*100 + b[[3]]*1

# as we saw earlier, res has an element called 'coefficients' and this is
# really just what coef() is extracting from res
res$coefficients

# extract the variance-covariance matrix of the coefficients
vcov(res)

# the square-root of the diagonal elements of this matrix are the standard
# errors of the estimated regression coefficients
sqrt(diag(vcov(res)))

# extract the residuals
resid(res)

# extract the fitted values
fitted(res)

# fitted values versus residuals plot
plot(fitted(res), resid(res), pch=19)
abline(h=0, lty="dotted")

# we can also directly call plot() on the model object
plot(res)

# this gives four different plots, the first is the fitted-versus-residuals
# plot, the second is a qq-plot of the residuals, the third is a plot of the
# fitted values versus the square-root of the absolute value of the
# standardized residuals, and the fourth is a plot of the 'leverage' of the
# points versus the standardized residuals; these different plots have
# different diagnostic purposes; to read a bit more about these plots (and
# others that can be created with plot() here), see:
help(plot.lm)

# above, we computed a predicted value manually; but we can use predict() to
# do the computations more conveniently
predict(res, newdata=data.frame(hp=100, am=1))

# with this, we can easily compute multiple predicted values simultaneously
predict(res, newdata=data.frame(hp=seq(60,200,by=20), am=1))

# look at the documentation of the predict method for lm objects
help(predict.lm)

# we see that we can get confidence intervals for the predictions by setting
# the 'interval' argument to "confidence" (with 95% CIs being the default)
predict(res, newdata=data.frame(hp=seq(60,200,by=20), am=1), interval="confidence")

# fit two models, one with just 'hp' as predictor and one with also 'am' as predictor
res0 <- lm(mpg ~ hp, data=mtcars)
res1 <- lm(mpg ~ hp + am, data=mtcars)

# model comparison (this is identical to testing the slope of 'am')
anova(res0, res1)
summary(res1)

# fit two models, one with just the intercept and one with both predictors
res0 <- lm(mpg ~ 1, data=mtcars)
res1 <- lm(mpg ~ hp + am, data=mtcars)

# sidenote: res0 is a model that doesn't take any of the characteristics of
# the cars into consideration when making a prediction about their gas
# mileage; or in other words, the estimated intercept of this model is equal
# to the average mpg of the 32 cars in the dataset, and hence it predicts the
# same gas mileage for all 32 cars

# model comparison (this is identical to the omnibus F-test of the res1 model)
anova(res0, res1)
summary(res1)

# compare two more models, one with just hp as predictor and the other one
# also with number of cylinders (as a factor / categorical predictor)
res0 <- lm(mpg ~ hp, data=mtcars)
res1 <- lm(mpg ~ hp + factor(cyl), data=mtcars)
anova(res0, res1)

# this is testing whether the cylinders factor as a whole is significant
# (while controlling for the horsepower of the cars)

# compare two more models, one assuming a linear relationship between hp and
# gas mileage and one assuming a non-linear relationship (of the form of a
# cubic polynomial)
res0 <- lm(mpg ~ hp, data=mtcars)
res1 <- lm(mpg ~ poly(hp, degree=3), data=mtcars)
anova(res0, res1)

# plot the data and add the regression lines from the two models to the plot
plot(mpg ~ hp, data=mtcars, pch=21, bg="lightgray")
abline(res0, lwd=3)
hps <- 40:350
pred <- predict(res1, newdata=data.frame(hp=hps))
lines(hps, pred, lwd=3, col="red")

# treat hp as a categorical predictor
res1 <- lm(mpg ~ factor(hp), data=mtcars)
summary(res1)

# this model gives the best fit among all models that only take hp into
# consideration for making predictions about gas mileage

# add the predicted values of this model to the plot
hps <- sort(unique(mtcars$hp))
pred <- predict(res1, newdata=data.frame(hp=hps))
lines(hps, pred, lwd=3, col="blue")

# now test if this model gives a significantly better fit than the model that
# assumes that the relationship is linear
anova(res0, res1)

# this is sometimes called the 'lack of linearity test' (which goes back to
# Fisher, 1922; https://doi.org/10.1111/j.2397-2335.1922.tb00832.x)

# fit a model with multiple predictors
res <- lm(mpg ~ hp + am + factor(cyl) + wt, data=mtcars)
summary(res)

# when we use anova() just on a single model like the one above, then we get a
# sequence of tests
anova(res)

############################################################################

## 11.4: Analysis of variance and model comparison

# a simple example of aov(): one-way analysis of variance (ANOVA)
res <- aov(mpg ~ factor(cyl), data=mtcars)
summary(res)

# using aov(), one can also fit more complex ANOVA-type models (e.g., with
# multiple between- and/or within-subject factors); depending on the model,
# this requires the use of Error() in the mode formula and can get more
# complex; a nice package that can simplify the fitting of such models is the
# 'ez' package: https://cran.r-project.org/package=ez

############################################################################

