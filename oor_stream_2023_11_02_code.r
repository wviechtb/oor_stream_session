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


############################################################################
