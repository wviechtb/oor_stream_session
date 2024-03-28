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

# change the plotting symbol to a filled circle, use gray as the background
# color of the circles, use only an L-shape around the plot, use horizontal
# axis labels, and add proper x-axis and y-axis labels
plot(mpg ~ hp, data=dat, pch=21, bg="gray", bty="l", las=1,
     xlab="Horsepower", ylab="Miles per Gallon")

# by setting the graphics parameters within the plot() function call, we only
# temporarily change the defaults, so when we create a new graph, it will
# revert to the defaults
plot(mpg ~ wt, data=dat, xlab="Weight", ylab="Miles per Gallon")

# 12.4.1: Permanent changes: The par() function

# list all graphics parameters (with their current values)
par()

# list only some selected graphics parameters
par(c("col", "lty"))

# set some graphics parameters
par(bty="l", las=1, pch=21)

# redraw the two graphs from above
plot(mpg ~ hp, data=dat, bg="gray", xlab="Horsepower", ylab="Miles per Gallon")
plot(mpg ~ wt, data=dat, bg="gray", xlab="Weight", ylab="Miles per Gallon")

# why don't we also use 'bg' above with par() to set the background color of
# the points? it turns out that the graphical parameter 'bg' is for setting
# the background color of the plotting device and that plot() *also* has a
# special argument called 'bg' for setting the background color of points (so
# these two things - the graphical parameter and the argument of plot() - are
# different things)

# to illustrate this distinction
par(bty="l", las=1, pch=21, bg="gray")
plot(mpg ~ hp, data=dat, bg="dodgerblue", xlab="Horsepower", ylab="Miles per Gallon")

# see help(par) for more details on all of the graphical parameters

# as noted there, some graphical parameters can only be set via par() and not
# within the call to the plotting function; a good example of this is 'mfrow'
# for splitting up the plotting device into multiple rows and/or columns
par(mfrow=c(1,2))
plot(mpg ~ hp, data=dat, bg="dodgerblue", xlab="Horsepower", ylab="Miles per Gallon")
plot(mpg ~ wt, data=dat, bg="dodgerblue", xlab="Weight", ylab="Miles per Gallon")

# note that the adjusted graphics parameters stay in effect for additional
# plots drawn on the same plotting device
plot(mpg ~ factor(cyl), data=dat)

# if we did not want two subplots, we set mfrow back to a single row/column
par(mfrow=c(1,1))
plot(mpg ~ factor(cyl), data=dat)

# the easiest way to reset of graphics parameters to their defaults is to
# close the plotting device (i.e., close the window) or via dev.off()
dev.off()

# now if we call a high-level plotting function that automatically opens a new
# plotting device (if none is already open), then the defaults are used again
plot(mpg ~ factor(cyl), data=dat)

# in RStudio, can also click on the broom symbol above the plot region to
# close the plotting device and hence reset adjusted graphical parameters

# the workflow using oldpar <- par(...) and then par(oldpar) is not something
# that I make use of (this can be relevant when writing plotting functions
# that adjust graphical parameters to reset things back when the function
# finishes doing its job, but in standard analysis scripts, using dev.off() or
# resetting graphical parameters directly as shown above with mfrow is simple
# enough)

# 12.4.2: Temporary changes: Arguments to graphics functions

# changing graphics parameters directly within the function call was
# illustrated already above; generally, this is also what I do in my workflow
# (except of course for graphics parameters that can only be set via par())

# as noted, there can be inconsistencies when doing so, as we also saw above
# with 'bg' (with par(), it sets the background color of the plotting device,
# within plot(), it sets the background color of filled plotting symbols)

############################################################################

## 12.5: Graphics parameters list

# 12.5.1: Graphical elements

# illustrate the pch argument
plot(1, 1, pch="+")

# not sure why the manual says that the symbol will be 'slightly above or
# below the appropriate position'; in this example, the + is exactly at the
# intersection of (1,1)
abline(h=1, lty="dotted")
abline(v=1, lty="dotted")

# can also use numbers for pch
plot(1, 1, pch=19)

# see help(points) for details on what the numbers mean
help(points)

# with the filled symbols, we can specify the background color
plot(1, 1, pch=21, bg="gray")

# can also specify the border color
plot(1, 1, pch=21, bg="gray", col="dodgerblue")

# illustrate different line types
abline(h=0.6, lty=1) # solid line (default)
abline(h=0.8, lty=2) # dashed line
abline(h=1.2, lty=3) # dotted line
abline(h=1.4, lty=4) # dot-dash line

############################################################################
