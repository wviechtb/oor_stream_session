############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-05-02
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 8.1 - ?
#
# last updated: 2024-05-02

############################################################################

# for the survey about topics for future streams (alternating with the coverage
# of the present book), go here: https://forms.gle/VEEKNRvquRCGKjb78

############################################################################

# - version R 4.4-0 released a week ago
# - security vulnerability in older version of R
#   - https://hiddenlayer.com/research/r-bitrary-code-execution/
#   - https://www.kb.cert.org/vuls/id/238194
#   - https://nvd.nist.gov/vuln/detail/CVE-2024-27322
#   - https://stat.ethz.ch/pipermail/r-help/2024-May/479281.html
# - sort_by() function

############################################################################

### 8.1: Least squares, maximum likelihood, and Bayesian inference

## Least squares

# simulate some data based on a simple regression model
set.seed(1239)
n <- 50
x <- runif(n, 0, 10)
y <- 2 + 0.5 * x + rnorm(n, mean=0, sd=1)

# plot the data
plot(x, y, pch=21, bg="gray")

# add the true regression line to the plot
abline(a=2, b=0.5, lwd=3)

# since we know the true regression line here, we can compute the errors
y - (2 + 0.5 * x)

# create the X and y matrices
X <- cbind(1, x)
X
y <- cbind(y)
y

# the regression model can then be written in matrix notation as:
#
# y = X %*% beta + e
#
# where beta is a column vector with the true intercept and slope

# now apply equation (8.2) to compute the estimated intercept and slope
betahat <- solve(t(X) %*% X) %*% t(X) %*% y
betahat

# double-check that this gives the same estimates as (8.3) and (8.4)
b <- sum((x - mean(x)) * y) / sum((x - mean(x))^2)
a <- mean(y) - b * mean(x)
rbind(a, b)

# add the estimated regression line to the plot
abline(a, b, lwd=3, lty="dotted")

# fit the model using lm()
res <- lm(y ~ x)
coef(res)

# aside from being tedious, it should be noted that the 'manual' computations
# above are not how the estimates of the intercept and slope should be computed;
# lm() internally uses equations that are numerically more stable

# compute the residuals
resid <- c(y - (a + b * x))
resid

# check that the mean of the residuals is zero (note: due to numerical
# imprecision, this value is not exactly equal to 0, but practically
# indistinguishable from zero)
mean(resid)

# compute the RSS
sum(resid^2)

# you cannot make this any smaller with other values of 'a' and 'b'

# function to compute the RSS for given values of 'a' and 'b'
rss <- function(x, y, a, b) {
   resid <- y - (a + b*x)
   return(sum(resid^2))
}

# double-check that we get the same RSS as above
rss(x, y, a, b)

# try a few other values for 'a' and 'b'
rss(x, y, a=2.3, b=0.41)
rss(x, y, a=2.1, b=0.41)
rss(x, y, a=2.1, b=0.40)

## Estimation of residual standard deviation sigma

# we know that the true sigma is equal to 1 (since we simulated the data), but
# in practice, we would not know this; instead, we can estimate sigma based on
# the residuals; we can do this by computing the square root of 1/n * RSS
sqrt(1/n * sum(resid^2))

# but a better estimate is to divide by n-2 (where the 2 is the number of
# regression coefficients of the model)
sqrt(1/(n-2) * sum(resid^2))




# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat", destfile="hibbs.dat")

# read in the data and inspect it
dat <- read.table("hibbs.dat", header=TRUE)
dat

############################################################################
