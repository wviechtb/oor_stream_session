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

# extract the residuals
resid(res)

# extract the fitted values
fitted(res)



############################################################################
