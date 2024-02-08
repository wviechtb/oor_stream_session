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

res <- lm(mpg ~ hp + wt + am, data=dat)
summary(res)

# fit the same model using glm()

res <- glm(mpg ~ hp + wt + am, family=gaussian, data=dat)
summary(res)

# do a median split on the mpg variable (1 = high mpg, 0 = low mpg), so that
# we have a dichotomous outcome variable for doing logistic regression

dat$highmpg <- ifelse(dat$mpg > median(dat$mpg), 1, 0)
dat



############################################################################

# - regression model with log transformed outcome
# - ZIP (zero-inflated Poisson model)

############################################################################
