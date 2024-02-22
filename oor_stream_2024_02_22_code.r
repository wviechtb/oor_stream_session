############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-02-22
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 11.6
#
# last updated: 2024-02-22

############################################################################

## 11.7: Nonlinear least squares and maximum likelihood models

# 11.7.1: Least squares

# example data from Bates & Watts (1988), page 51
x <- c(0.02, 0.02, 0.06, 0.06, 0.11, 0.11, 0.22, 0.22, 0.56, 0.56, 1.10, 1.10)
y <- c(76, 47, 97, 107, 123, 139, 159, 152, 191, 201, 207, 200)

# scatterplot of x versus y
plot(x, y, pch=21, bg="gray", cex=1.5, ylim=c(40,220))

# naive fit a simple linear regression model
res1 <- lm(y ~ x)
summary(res1)

# compute predicted values based on the model for values of x between 0 and 1.2
xs <- seq(0, 1.2, length=100)
pred <- predict(res1, newdata=data.frame(x=xs))

# add the regression line based on the predicted values to the plot
lines(xs, pred, lwd=3, col="black")

# fit a quadratic polynomial regression model
res2 <- lm(y ~ x + I(x^2))
summary(res2)

# compute predicted values based on the model for values of x between 0 and 1.2
xs <- seq(0, 1.2, length=100)
pred <- predict(res2, newdata=data.frame(x=xs))

# add the regression line based on the predicted values to the plot
lines(xs, pred, lwd=3, col="blue")

# fit a cubic polynomial regression model
res3 <- lm(y ~ x + I(x^2) + I(x^3))
summary(res3)

# compute predicted values based on the model for values of x between 0 and 1.2
xs <- seq(0, 1.2, length=100)
pred <- predict(res3, newdata=data.frame(x=xs))

# add the regression line based on the predicted values to the plot
lines(xs, pred, lwd=3, col="red")

# non-linear function that defines the shape of the relationship between x and y
predfun <- function(beta, x) beta[1] * x / (beta[2] + x)

# looking at the scatterplot, we see that y should be around 60 when x=0

# when x=0.1, then y should be around 100
# when x=1.0, then y should be around 200
#
# therefore:
#
# beta[1] * 0.1 / (beta[2] + 0.1) =~ 100
# beta[1] * 1.0 / (beta[2] + 1.0) =~ 200
#
# beta[1] * 0.1 = 100 * (beta[2] + 0.1)
# beta[1] = 100 * (beta[2] + 0.1) / 0.1
#
# 100 * (beta[2] + 0.1) / 0.1 * 1.0 / (beta[2] + 1.0) = 200
# 10 * (beta[2] + 0.1) / (beta[2] + 1.0) = 200
# (beta[2] + 0.1) / (beta[2] + 1.0) = 20
# (beta[2] + 0.1) = 20 * (beta[2] + 1.0)
# beta[2] + 0.1 = 20 * beta[2] + 20
# beta[2] - 20 * beta[2] = 20 - 0.1
# -19 * beta[2] = 19.9
# beta[2] = -19.1 / 19
#
# beta[1] * 0.1 / (-19.1 / 19 + 0.1) = 100
# beta[1] = 100 / 0.1 * (-19.1 / 19 + 0.1)



# fit function
fn <- function(beta, x, y) sum((y - (beta[1] * x) / (beta[2] + x))^2)
