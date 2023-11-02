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
summary(res)

############################################################################
