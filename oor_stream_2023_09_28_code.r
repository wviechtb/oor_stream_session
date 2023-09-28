############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-09-28
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 4.2 - ?
#
# last updated: 2023-09-28

############################################################################

### 4.2: Estimates, standard errors, and confidence intervals

## Standard error for a comparison

# generate two vectors of data corresponding to the given example

x.m <- sample(c(rep(1,228), rep(0,172)))
x.m
prop.m <- mean(x.m)
prop.m

x.w <- sample(c(rep(1,270), rep(0,330)))
x.w
prop.w <- mean(x.w)
prop.w

se.m <- sqrt(prop.m * (1-prop.m) / length(x.m))
se.w <- sqrt(prop.w * (1-prop.w) / length(x.w))