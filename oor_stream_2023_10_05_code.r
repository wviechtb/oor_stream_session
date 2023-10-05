############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-10-05
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 11.2 - ?
#
# last updated: 2023-10-05

############################################################################

### 11.1: Defining statistical models; formulae

# before we continue, let's look at some examples with categorical predictors

# compare the gas mileage of cars with an automatic versus manual transmission
res <- lm(mpg ~ am, data=mtcars)
summary(res)

# we are fitting this model:
#
# mpg = beta0 + beta1 * am + error
#
# where am = 0 for cars with an automatic transmission and am = 1 for cars
# with a manual transmission, so the intercept is the expected mpg for cars
# with an automatic transmission and the slope for am is the mean difference
# in mpg for cars with a manual transmission compared to cars with an
# automatic transmission

# say am was coded not as a dummy variable, but as a string variable
mtcars$transmission <- ifelse(mtcars$am == 1, "manual", "automatic")
mtcars

# use such a string in the model
res <- lm(mpg ~ transmission, data=mtcars)
summary(res)

# the results are identical; the 'transmission' variable was turned into a
# 'factor' which then gets dummy-coded for inclusion in the model

# as we saw in section 4, we can manually turn a variable into a factor with
# the factor() function
factor(mtcars$transmission)

# we can also do the dummy-coding manually with the model.matrix() function
model.matrix(~ factor(mtcars$transmission))

# so the 0 refers to the first level (automatic) and the 1 refers to the
# second level (manual)

# we can also include a factor directly in the model as a predictor
mtcars$transmission <- factor(mtcars$transmission)
res <- lm(mpg ~ transmission, data=mtcars)
summary(res)

# sidenote: this is the same as running a classical Student's t-test (assuming
# equal variances within the two groups)
t.test(mpg ~ am, data=mtcars, var.equal=TRUE)

# scatterplot of mpg (miles per gallon) on the y-axis and cyl (number of
# cylinders) on the x-axis
plot(mpg ~ cyl, data=mtcars, pch=21, bg="lightgray", cex=1.5,
     xlab="Number of Cylinders", ylab="Mile per Gallon")

# regression model with number of cylinders as a numeric variable
res <- lm(mpg ~ cyl, data=mtcars)
summary(res)
abline(res, lwd=3)

# inspect the corresponding model matrix
model.matrix(res)

# regression model where we treat cyl as a factor (categorical variable)
res <- lm(mpg ~ factor(cyl), data=mtcars)
summary(res)

# inspect the corresponding model matrix
model.matrix(res)

# so the model is given by this equation:
#
# mpg = beta0 + beta1 * factor(cyl)6 + beta2 * factor(cyl)8 + error
#
# where factor(cyl)6 is 1 for cars with 6 cylinders, factor(cyl)8 is 1 for
# cars with 8 cylinders, and both are 0 for cars with 4 cylinders; so the
# intercept is the expected mpg for cars with 4 cylinders; beta1 is the mean
# difference in mpg for cars with 6 cylinders versus cars with 4 cylinders and
# beta2 is the mean difference in mpg for cars with 8 versus 4 cylinders

# sidenote: this is the same as running an ANOVA (we will get to aov() later)
summary(aov(mpg ~ factor(cyl), data=mtcars))

# get the predicted (expected) mpg for each level of cyl from the model
newdat <- data.frame(cyl=c(4,6,8))
pred <- predict(res, newdata=newdat)
pred

# these are in fact just the means of cars falling within these three groups
by(mtcars$mpg, mtcars$cyl, mean)

# add the predicted values to the plot
points(newdat$cyl, pred, pch=19, cex=2.5, type="o", lty="dotted", lwd=3)

# we notice that the predicted values are quite similar for the two models; so
# does the model that treats cylinders as a categorical variable give us a
# significantly better fit?
res1 <- lm(mpg ~ cyl, data=mtcars)
res2 <- lm(mpg ~ factor(cyl), data=mtcars)
anova(res1, res2)

# since the model comparison F-test is not significant, the fit of the model
# that treats cylinders categorically is not significantly better

# we can estimate the mean difference in mpg for cars with 8 versus 6
# cylinders from this by taking the difference between the corresponding
# coefficients; but how can we get a test of this difference?
coef(res)[3] - coef(res)[2]

# for this, we will make use of the 'multcomp' package, so install the package
# first if you do not already have it installed
#install.packages("multcomp")

# load the 'multcomp' package
library(multcomp)

# we want test the following linear combination of the coefficients:
# (0) * beta0 + (-1) * beta1 + (1) * beta2 = beta2 - beta1

# construct the corresponding matrix with the multipliers
rbind(c(0,-1,1))

# we can use this together with glht() to test this linear combination
summary(glht(res, rbind(c(0,-1,1))), test=adjusted("none"))

# note: since we are only testing a single linear combination, whether the
# p-value given is adjusted for multiple testing or not makes no difference,
# but note that by default, the p-value is adjusted, which matters when
# testing multiple such linear combinations at the same time

# we can also make use of the 'car' package, so install the package first if
# you do not already have it installed
#install.packages("car")

# load the 'car' package
library(car)

# we can again test the linear combination of the coefficients as above
linearHypothesis(res, hypothesis.matrix=rbind(c(0,-1,1)))

# by default, the 'reference level' is the value of the variable that is
# alpha-numerically the lowest
factor(mtcars$cyl)

# but we can change the reference level with relevel()
relevel(factor(mtcars$cyl), ref="6")

# or when we create the factor, we specify the levels in the desired order,
# with the first denoting the reference level
factor(mtcars$cyl, levels=c("6","4","8"))

# same regression model as above but with 6 cylinders as the reference level
res <- lm(mpg ~ relevel(factor(cyl), ref="6"), data=mtcars)
summary(res)

# in the output, the names of the predictor variables becomes quite long this
# way, so let's first create the releveled factor and then include it in the
# model
mtcars$fcyl <- relevel(factor(mtcars$cyl), ref="6")
res <- lm(mpg ~ fcyl, data=mtcars)
summary(res)

# let's go back to the case where the reference level is 4 cylinders
mtcars$fcyl <- factor(mtcars$cyl)
res <- lm(mpg ~ fcyl, data=mtcars)
summary(res)

# regression model with a factor as predictor and we remove the intercept term
res <- lm(mpg ~ 0 + fcyl, data=mtcars)
summary(res)
model.matrix(res)

# the model fitted is given by this equation:
#
# mpg = beta1 * fcyl4 + beta2 * fcyl6 + beta3 * fcyl8 + error
#
# where fcyl4 = 1 for cars with 4 cylinders, fcyl6 = 1 for cars with 6
# cylinders, and fcyl8 = 1 for cars with 8 cylinders, so beta1 is the expected
# mpg for cars with 4 cylinders, beta2 is the expected mpg for cars with 6
# cylinders, and beta3 is the expected mpg for cars with 8 cylinders

# we see again that these are the mpg means of the three groups
by(mtcars$mpg, mtcars$cyl, mean)

# get the same contrasts as from the model with the intercept term
summary(glht(res, rbind(c(-1,1,0))), test=adjusted("none"))
summary(glht(res, rbind(c(-1,0,1))), test=adjusted("none"))

# and we can get again the contrast between 8 and 6 cylinders
summary(glht(res, rbind(c(0,-1,1))), test=adjusted("none"))

# we can obtain all three contrasts in a single line of code; now we will also
# adjust the p-values for multiple testing using the Bonferroni method
summary(glht(res, rbind(c(-1,1,0),c(-1,0,1),c(0,-1,1))), test=adjusted("bonferroni"))

# we can also get the result of the omnibus test from the model with the
# intercept term from the model without the intercept term, by simultaneously
# testing the contrasts of 6 versus 4 cylinders and 8 versus 4 cylinders
linearHypothesis(res, hypothesis.matrix=rbind(c(-1,1,0),c(-1,0,1)))

############################################################################

## 11.1.1 Contrasts