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

## 11.1.1 Contrasts