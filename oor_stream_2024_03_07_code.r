############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-03-07
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 12.1
#
# last updated: 2024-03-07

############################################################################

### 12: Graphical procedures

############################################################################

## 12.1: High-level plotting commands

# 12.1.1: The plot() function

# copy the mtcars dataset to dat and inspect it
dat <- mtcars
dat

# create a scatterplot of mpg (y-axis) versus hp (x-axis)
plot(dat$hp, dat$mpg)

# can also provide a matrix with two colums to create the same plot
plot(cbind(dat$hp, dat$mpg))

# can also provide a two variable data frame
plot(dat[c("hp", "mpg")])

# examine the AirPassengers dataset
AirPassengers

# this is a time series dataset (class 'ts')
class(AirPassengers)

# plot the time series
plot(AirPassengers)

# plot a single numeric vector (then the x-axis is the index of the points)
plot(dat$mpg)

# when the variable is a factor, then a bar plot is produced (of the
# frequencies of the various levels of the factor)
plot(factor(dat$cyl))

# if we pass a factor and a numeric variable to plot(), we get a boxplot for
# each level of the factor
plot(factor(dat$cyl), dat$mpg)

# when passing an entire data frame, we get a scatterplot matrix
plot(dat)

# we can pick out the variables we want to plot against each other
plot(~ mpg + hp + wt, data=dat)

# we can also create a scatterplot using this kind of formula notation
plot(mpg ~ hp, data=dat)

# if we have multiple variables on the right-hand side of the formula, then R
# prompts us to hit return to see each plot in turn
plot(mpg ~ hp + wt, data=dat)

# 12.1.2: Displaying multivariate data

# can also use the pairs() function to create a scatterplot matrix
pairs(dat)

# the pairs() function provides some additional functionality for customizing
# such plots; see help(pairs) for details and examples

# create a conditioning plot of mpg versus hp for every cyl level
coplot(mpg ~ hp | factor(cyl), data=dat)

# do not show the 'single bars' at the top, use a single row, and filled circles
coplot(mpg ~ hp | factor(cyl), data=dat, show.given=FALSE, rows=1, pch=19)

# could also condition on the combination of two variables
coplot(mpg ~ hp | factor(cyl) + factor(gear), data=dat)

# condition on a quantitative variable; then the function creates (by default,
# somewhat overlapping) intervals for the conditioning variable (number is
# used to control how many such intervals are created; 6 by default)
coplot(mpg ~ hp | wt, data=dat, number=4)

# use a different function for the panels; 'panel.smooth' shows the points and
# then uses a smoother to show the relationship between x and y
coplot(mpg ~ hp | wt, data=dat, number=4, panel=panel.smooth)

# can write a custom panel function, for example one that shows the points and
# adds the regression line from a simple linear regression model

panel.reg <- function(x, y, ...) {
   points(x, y)
   res <- lm(y ~ x)
   abline(res, lwd=3)
}

coplot(mpg ~ hp | wt, data=dat, number=4, panel=panel.reg)

# 12.1.3: Display graphics

# fit a linear regression model predicting mpg from hp
res <- lm(mpg ~ hp, data=dat)

# extract the residuals and create a normal QQ-plot based on them
resids <- resid(res)
qqnorm(resids)
qqline(resids)

# create a histogram of the residuals
hist(resids)

# can specify the number of breaks via the 'breaks' argument (same as nclass);
# this is a bit silly here with such a small number of datapoints
hist(resids, breaks=20)

# can also specify the exact location of the break points
hist(resids, breaks=seq(floor(min(resids)),ceiling(max(resids)),by=1))

# dotchart of the mpg values (with the car names added as labels)
dotchart(dat$mpg, labels=rownames(dat))

# fit a regression model predicting mpg from wt and hp and also use quadratic
# terms for each predictor
res <- lm(mpg ~ wt + I(wt^2) + hp + I(hp^2), data=dat)
summary(res)

# compute the predicted mpg value for combinations of wt and hp (within the
# range of the observed data)

wts <- seq(min(dat$wt), max(dat$wt), length=100)
hps <- seq(min(dat$hp), max(dat$hp), length=100)

pred <- outer(wts, hps, function(x, y) {
   coef(res)[1] + coef(res)[2]*x + coef(res)[3]*x^2 + coef(res)[4]*y + coef(res)[5]*y^2
})

# create a contour plot showing the predicted mpg as a function of wt and hp
contour(wts, hps, pred, xlab="Weight", ylab="Horse Power")

############################################################################


