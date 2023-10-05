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
# with an automatic transmission



## 11.1.1 Contrasts