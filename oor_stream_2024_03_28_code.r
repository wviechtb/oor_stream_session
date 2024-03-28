############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-03-28
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 12.4 - ?
#
# last updated: 2024-03-28

############################################################################

## 12.4: Using graphics parameters

# copy the mtcars dataset to dat
dat <- mtcars
dat

# a default looking scatterplot
plot(mpg ~ hp, data=dat)

# change the plotting symbol to a filled circle and use gray as the background
# color of the circles, use only an L-shape around the plot, and add proper
# x-axis and y-axis labels
plot(mpg ~ hp, data=dat, pch=21, bg="gray", bty="l", xlab="Horsepower",
     ylab="Miles per Gallon")

# by setting the graphics parameters within the plot() function call, we only
# temporarily change the defaults, so when we create a new graph, it will
# revert to the defaults
plot(mpg ~ wt, data=dat)

# 12.4.1: Permanent changes: The par() function

# list all graphics parameters (with their current values)
par()

############################################################################
