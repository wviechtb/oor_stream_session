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


############################################################################


