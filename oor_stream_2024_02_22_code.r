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

# when x=0.1, then y should be around 125
# when x=1.0, then y should be around 200
#
# therefore:
#
# beta[1] * 0.1 / (beta[2] + 0.1) =~ 125
# beta[1] * 1.0 / (beta[2] + 1.0) =~ 200
#
# beta[1] * 0.1 = 125 * (beta[2] + 0.1)
# beta[1] = 125 * (beta[2] + 0.1) / 0.1
# beta[1] = 1250 * (beta[2] + 0.1)
#
# 1250 * (beta[2] + 0.1) * 1.0 / (beta[2] + 1.0) = 200
# 1250 * (beta[2] + 0.1) / (beta[2] + 1.0) = 200
# (beta[2] + 0.1) / (beta[2] + 1.0) = 0.16
# (beta[2] + 0.1) = 0.16 * (beta[2] + 1.0)
# beta[2] + 0.1 = 0.16 * beta[2] + 0.16
# beta[2] - 0.16 * beta[2] = 0.16 - 0.1
# 0.84 * beta[2] = 0.06
# beta[2] = 0.06 / 0.84
# beta[2] = 0.07142857
#
# beta[1] * 0.1 / (0.07142857 + 0.1) = 100
# beta[1] = 100 / 0.1 * (0.07142857 + 0.1)
# beta[1] = 171.4286

### DOUBLE CHECK THE ABOVE!!!

pred <- predfun(beta=c(171.4286, 0.07142857), x=xs)
lines(xs, pred, lwd=3, col="green")

# fit function (we want to find the values of beta[1] and beta[2] that
# minimizes the sum of the squared deviations)
fn <- function(beta, x, y) {
   pred <- (beta[1] * x) / (beta[2] + x)
   sum((y - pred)^2)
}

# compute the fit value given the rough estimates we obtained above
fn(beta=c(171.4286, 0.07142857), x=x, y=y)

# try out some other rough estimates for beta[1] and beta[2]
fn(beta=c(200, 0.1), x=x, y=y)
fn(beta=c(200, 0.08), x=x, y=y)
fn(beta=c(200, 0.05), x=x, y=y)
# ...

# use nlm() to find the values for beta[1] and beta[2] that give the best fit
res <- nlm(fn, p=c(200, 0.1), hessian=TRUE, x=x, y=y, print.level=2)
res

# redraw the scatterplot of x versus y
plot(x, y, pch=21, bg="gray", cex=1.5, ylim=c(40,220))

# compute the predicted values based on the parameter estimates found and add
# the regression 'line' to the plot
pred <- predfun(beta=res$estimate, x=xs)
lines(xs, pred, lwd=3, col="green")

############################################################################

beta1s <- seq(170, 250, length=100)
beta2s <- seq(0.05, 0.1, length=100)

ssemat <- matrix(NA, nrow=length(beta1s), ncol=length(beta2s))

for (i in 1:length(beta1s)) {
   for (j in 1:length(beta1s)) {
      ssemat[i,j] <- fn(beta=c(beta1s[i], beta2s[j]), x=x, y=y)
   }
}

tmp <- persp(x=beta1s, y=beta2s, z=ssemat, xlab="beta1", ylab="beta2", zlab="SSE",
             col="gray80", border="gray50", ticktype="detailed",
             theta=135, phi=25, shade=0.7, ltheta=60)

cords <- trans3d(x=200, y=0.1, z=fn(beta=c(200, 0.1), x=x, y=y), pmat=tmp)
points(cords$x, cords$y, pch=19, cex=2)

# find the row and column number for the smallest value in the matrix
loc <- which(ssemat == min(ssemat), arr.ind = TRUE)
loc

beta1s[loc[1]]
beta2s[loc[2]]

cords <- trans3d(x=beta1s[loc[1]], y=beta2s[loc[2]], z=min(ssemat), pmat=tmp)
points(cords$x, cords$y, pch=19, cex=2)

# taking the inverse of the Hessian matrix gives an estimate of the
# variance-covariance matrix of the parameter estimates
V <- solve(res$hessian)
V

# the square root of the diagonal elements are the standard errors of the estimates
se <- sqrt(diag(V))
se

tab <- data.frame(beta = res$estimate, se = se)
tab$zval <- tab$beta / tab$se
tab

# https://en.wikipedia.org/wiki/Observed_information

############################################################################

predfun <- function(beta, x) beta[1] + beta[2] * x / (beta[3] + x)

fn <- function(beta, x, y) {
   pred <- beta[1] + (beta[2] * x) / (beta[3] + x)
   sum((y - pred)^2)
}

res <- nlm(fn, p=c(0, 200, 0.1), hessian=TRUE, x=x, y=y)
res

pred <- predfun(beta=res$estimate, x=xs)
lines(xs, pred, lwd=3, col="purple")

############################################################################
